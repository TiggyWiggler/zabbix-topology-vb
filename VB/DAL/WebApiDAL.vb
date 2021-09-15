Imports System.IO
Imports System.Net
Imports System.Text
Imports Newtonsoft.Json
Imports Newtonsoft.Json.Linq

Public Class WebApiDAL : Implements ZabbixDal

    Private Const ApiUrl = "http://192.168.68.125/api_jsonrpc.php"
    Private Shared AuthKey As String          ' Authentication key to be used in each request.
    Private Shared RequestId As Integer = 0        ' Must be unique for each request, so incremement on each request.
    Private ImageCache As List(Of RetrievedImage) = Nothing
    Private CacheResponsesToStore As Boolean = True       ' Should we cache Zabbix responses to the file cache?

    ''' <summary>
    ''' Cache the response to the file store (if one found)
    ''' </summary>
    ''' <param name="Response"></param>
    Private Sub CacheResponse(ByVal Response As JArray, ByVal Name As String)
        ' We will create folders with the current date and time in the format YYYYMMDDHHmm and assume that the minute does not roll over during the 
        ' cache of subsequent requests. 
        Dim TargetFolderName As String = DateTime.Now().ToString("yyyyMMddHHmm")
        Dim RootFolderName As String = IO.Path.Combine(My.Application.Info.DirectoryPath, "FileCache")

        If (Not System.IO.Directory.Exists(RootFolderName)) Then
            System.IO.Directory.CreateDirectory(RootFolderName)
        End If

        Dim RootInfo As New DirectoryInfo(RootFolderName)
        Dim TargetFolderInfo As DirectoryInfo = Nothing
        If RootInfo.Exists Then
            TargetFolderInfo = New DirectoryInfo(IO.Path.Combine(RootFolderName, TargetFolderName))
            If Not TargetFolderInfo.Exists Then
                ' Target folder does not exist, create it
                TargetFolderInfo = RootInfo.CreateSubdirectory(TargetFolderName)
            End If
            If Not TargetFolderInfo.Exists Then
                ' If it still does not exist then something was wrong creating it.
                Throw New ApplicationException("Target cache folder could not be created")
            End If
            ' Write the cache file out
            IO.File.WriteAllText(IO.Path.Combine(TargetFolderInfo.FullName, String.Concat(Name, ".jarray")), JsonConvert.SerializeObject(Response))
        Else
            Throw New ApplicationException("File cache root does not exist")
        End If
    End Sub

    Public Function IsAuthenticated() As Boolean Implements ZabbixDal.IsAuthenticated
        Return AuthKey <> ""
    End Function

    Public Sub Authenticate(ByVal UserName As String,
                            ByVal Password As String) Implements ZabbixDal.Authenticate
        If UserName = "" Then
            Throw New ApplicationException("Username is missing")
        End If
        If Password = "" Then
            Throw New ApplicationException("Password is missing")
        End If

        AuthenticateUser(UserName, Password)

    End Sub

    ''' <summary>
    ''' Authenticate against Zabbix server
    ''' </summary>
    ''' <param name="UserName"></param>
    ''' <param name="Password"></param>
    Private Sub AuthenticateUser(UserName As String, Password As String)

        Dim AuthParams As New AuthenticationParams
        With AuthParams
            .user = UserName
            .password = Password
        End With

        Dim HostResponse As ZabbixResponse = GetZabbixResponse("user.login", AuthParams)

        If IsNothing(HostResponse) Then
            Throw New ApplicationException("No authentication response received. Check server accessibility")
        ElseIf Not IsNothing(HostResponse.error) Then
            Throw New ApplicationException(HostResponse.error.data)
        Else
            AuthKey = HostResponse.result.ToString()
        End If

    End Sub

    Public Function GetMaps() As List(Of Map) Implements ZabbixDal.GetMaps
        Dim Params As New MapParams
        Params.output = "extend"
        Dim Response As ZabbixResponse = GetZabbixResponse("map.get", Params)

        If IsNothing(Response) Then
            Throw New ApplicationException("no map response received")
        ElseIf Not IsNothing(Response.error) Then
            Throw New ApplicationException(Response.error.data)
        ElseIf Not IsNothing(Response.result) Then
            Dim MapArray As JArray = Response.result
            If CacheResponsesToStore Then
                CacheResponse(MapArray, "map")
            End If
            Return MapArray.ToObject(Of List(Of Map))
        Else
            Return Nothing
        End If
    End Function

    Public Sub CreateMap(Map As Map) Implements ZabbixDal.CreateMap
        Dim Response As ZabbixResponse = GetZabbixResponse("map.create", Map)
        If IsNothing(Response) Then
            Throw New ApplicationException("No map create response received.")
        ElseIf Not IsNothing(Response.error) Then
            Throw New ApplicationException(Response.error.data)
        End If
    End Sub

    Public Sub DeleteMap(SysMapId As Integer) Implements ZabbixDal.DeleteMap
        Dim SysMapIds As String() = {SysMapId}
        Dim Response As ZabbixResponse = GetZabbixResponse("map.delete", SysMapIds)
        If IsNothing(Response) Then
            Throw New ApplicationException("No map delete response received.")
        ElseIf Not IsNothing(Response.error) Then
            Throw New ApplicationException(Response.error.data)
        End If
    End Sub

    Public Function ImageId(ByVal ImageName As String) As Integer Implements ZabbixDal.ImageId
        ' GetImages internally caches the result it gets from the web API so no need to store it to automatic variable here.
        Dim ret As Integer
        ret = GetImages.Where(Function(x) x.name = ImageName).Select(Function(x) CInt(x.imageid)).FirstOrDefault()
        If ret = 0 Then
            ' Default if nothing found
            If GetImages.Count > 0 Then
                ret = GetImages(0).imageid
            Else
                Throw New ApplicationException("No icons retrieved")
            End If
        End If
        Return ret
    End Function

    ''' <summary>
    ''' Get list of images from Zabbix
    ''' </summary>
    ''' <returns>Does not return the image data</returns>
    Private Function GetImages() As List(Of RetrievedImage)
        If IsNothing(ImageCache) Then
            Dim params As New ImageParams
            With params
                .output = "extend"
            End With

            Dim Response As ZabbixResponse = GetZabbixResponse("image.get", params)

            If IsNothing(Response) Then
                Throw New ApplicationException("no image response received")
            ElseIf Not IsNothing(Response.error) Then
                Throw New ApplicationException(Response.error.data)
            ElseIf Not IsNothing(Response.result) Then
                Dim ImageArray As JArray = Response.result
                If CacheResponsesToStore Then
                    CacheResponse(ImageArray, "images")
                End If
                ImageCache = ImageArray.ToObject(Of List(Of RetrievedImage))
                Return ImageCache
            Else
                Return Nothing
            End If
        Else
            Return ImageCache
        End If
    End Function

    Public Function GetHosts() As List(Of HostDevice) Implements ZabbixDal.GetHosts

        Dim HostJsonObjects As List(Of Host) = GetHostJsonObjects()     ' deserialise the JSON response from the server.

        Dim ret As List(Of HostDevice) = Nothing
        Dim Hd As HostDevice
        Dim Msap As Integer         ' Device Media Service Access Point
        Dim RemoteDevice As RemoteDevice

        For Each H As Host In HostJsonObjects
            Hd = New HostDevice
            Hd.HostName = H.host
            Hd.HostDeviceId = H.hostid      ' Take h.hostID into both HostId and HostDeviceId
            Hd.HostId = H.hostid

            ' host IP addresses
            If Not IsNothing(H.interfaces) AndAlso H.interfaces.Count > 0 Then
                For Each i As HostInterface In H.interfaces
                    If i.ip <> "" Then
                        If IsNothing(Hd.IpAddresses) Then
                            Hd.IpAddresses = New List(Of String)
                        End If
                        Hd.IpAddresses.Add(i.ip)
                    End If
                Next
            End If

            For Each Hi As HostItem In H.items
                Msap = MsapId(Hi.name)      ' MSAP is unique for each remote device and comes from collected LLDP data.
                If Msap > 0 Then
                    ' Connection to remote device
                    RemoteDevice = Nothing
                    If Not IsNothing(Hd.RemoteDevices) Then
                        For Each Rd As RemoteDevice In Hd.RemoteDevices
                            If Rd.MSAP = Msap Then
                                RemoteDevice = Rd
                                Exit For
                            End If
                        Next
                    End If

                    If IsNothing(RemoteDevice) Then
                        ' We have not started to add the remote device to this host device yet. Add it now.
                        RemoteDevice = New RemoteDevice
                        With RemoteDevice
                            .MSAP = Msap
                            .LocalPortName = PortName(Hi.name)
                        End With

                        ' We could do this earlier, but if we had found the remote device in the previous cycle we would have grabbed a reference, 
                        ' so this way everything after this If.. End If is the same no matter how the Remote Device rference was obtained.
                        If IsNothing(Hd.RemoteDevices) Then
                            Hd.RemoteDevices = New List(Of RemoteDevice)
                        End If
                        Hd.RemoteDevices.Add(RemoteDevice)
                    End If

                    If InStr(Hi.name, "Chassis Info Type") Then
                        RemoteDevice.ChassisIdType = Hi.lastvalue
                    ElseIf InStr(Hi.name, "Chassis Info") Then
                        RemoteDevice.ChassisId = Hi.lastvalue
                    ElseIf InStr(Hi.name, "Host Desc") Then
                        RemoteDevice.HostDescription = Hi.lastvalue
                    ElseIf InStr(Hi.name, "Host") Then
                        RemoteDevice.Host = Hi.lastvalue
                    ElseIf InStr(Hi.name, "Interface Info Type") Then
                        RemoteDevice.PortIdType = Hi.lastvalue
                    ElseIf InStr(Hi.name, "Interface Info") Then
                        RemoteDevice.PortId = Hi.lastvalue
                    ElseIf InStr(Hi.name, "Interface Desc") Then
                        RemoteDevice.Description = Hi.lastvalue
                    End If

                    ' Save the changes back.
                    Hd.RemoteDevices(Hd.RemoteDevices.IndexOf(RemoteDevice)) = RemoteDevice
                Else
                    ' Not a connection to a remote device. May find local data in this item.
                    If InStr(Hi.name, "Chassis Id Type") Then
                        Hd.ChassisType = Hi.lastvalue
                    ElseIf InStr(Hi.name, "Chassis Id") Then
                        Hd.ChassisId = Hi.lastvalue
                    End If
                End If
            Next
            If IsNothing(ret) Then
                ret = New List(Of HostDevice)
            End If
            ret.Add(Hd)
        Next

        Return ret
    End Function

    ''' <summary>
    ''' Extracts the MSAP (Device Media Service Access Point) from a string.
    ''' </summary>
    ''' <param name="Input">string containing MSAP in templated format from Zabbix</param>
    ''' <returns>-1 if nothing found, else the retrieved value</returns>
    Private Function MsapId(ByVal Input As String) As Integer
        Dim ret As Integer = -1

        Dim Prefix As String = "[MSAP "
        Dim Suffix As String = "]"
        Dim PrefixPos As Integer
        Dim SuffixPos As Integer
        Dim ValueString As String
        Dim StartPos As Integer
        Dim Length As Integer

        PrefixPos = InStr(Input, Prefix, CompareMethod.Text)
        If PrefixPos > 0 Then
            SuffixPos = InStr(PrefixPos, Input, Suffix)
            If PrefixPos > 0 And SuffixPos > 0 Then
                StartPos = PrefixPos + Len(Prefix) - 1
                Length = SuffixPos - (PrefixPos + Len(Prefix))
                If StartPos > 0 And Length > 0 And StartPos + Length <= Len(Input) Then
                    ' Extract the numeric component of the string
                    ValueString = Input.Substring(StartPos, Length)
                    ' Convert to an integer
                    Integer.TryParse(ValueString, ret)
                End If
            End If
        End If

        Return ret
    End Function

    ''' <summary>
    ''' Extract the port ID from a string
    ''' </summary>
    ''' <param name="Input">string containing port ID in templated format from Zabbix</param>
    ''' <returns>blank string if nothing found</returns>
    Private Function PortName(ByVal Input As String) As String
        Dim Prefix As String = "[Port - "
        Dim Suffix As String = "]"
        Dim PrefixPos As Integer
        Dim SuffixPos As Integer
        Dim StartPos As Integer
        Dim Length As Integer

        PrefixPos = InStr(Input, Prefix, CompareMethod.Text)
        If PrefixPos > 0 Then
            SuffixPos = InStr(PrefixPos, Input, Suffix)
            If PrefixPos > 0 And SuffixPos > 0 Then
                StartPos = PrefixPos + Len(Prefix) - 1
                Length = SuffixPos - (PrefixPos + Len(Prefix))
                If StartPos > 0 And Length > 0 And StartPos + Length <= Len(Input) Then
                    ' Extract the required component of the string
                    Return Input.Substring(StartPos, Length)
                End If
            End If
        End If
        Return ""
    End Function

    ''' <summary>
    ''' Get all hosts in the server
    ''' </summary>
    ''' <returns></returns>
    Private Function GetHostJsonObjects() As List(Of Host)

        Dim HostParams As New HostParams
        With HostParams
            .output = New String() {"hostid", "host"}
            .selectInterfaces = New String() {"interfaceid", "ip"}
            .selectItems = New String() {"itemid", "name", "lastvalue", "value_type"}

        End With

        Dim HostResponse As ZabbixResponse = GetZabbixResponse("host.get", HostParams)

        If IsNothing(HostResponse) Then
            Throw New ApplicationException("No host response received.")
        ElseIf Not IsNothing(HostResponse.error) Then
            Throw New ApplicationException(HostResponse.error.data)
        Else
            ' We have a list of hosts.
            Dim HostsArray As JArray = HostResponse.result
            If CacheResponsesToStore Then
                CacheResponse(HostsArray, "hosts")
            End If
            Dim Hosts As List(Of Host) = HostsArray.ToObject(Of List(Of Host))
            Return Hosts
        End If
    End Function

    ''' <summary>
    ''' Supply a request and get the response back.
    ''' </summary>
    ''' <returns></returns>
    Private Function GetZabbixResponse(Method As String, Params As Object) As ZabbixResponse
        RequestId += 1

        Dim Content As New ZabbixRequest
        With Content
            .method = Method
            .auth = AuthKey
            .id = RequestId
            .jsonrpc = "2.0"
            .params = Params
        End With

        Dim Request As HttpWebRequest
        Request = WebRequest.Create(ApiUrl)
        Request.Method = "POST"
        Request.ContentType = "application/json"

        Dim Js As New JsonSerializer
        Dim Sb As New StringBuilder

        ' Serialise the Content object as JSON into a StringBuilder 
        Using Sw As New StringWriter(Sb)
            Using Jw As New JsonTextWriter(Sw)
                Js.Serialize(Jw, Content)
            End Using
        End Using

        ' Convert StringBuilder content to byte array
        Dim dataArray() As Byte = Encoding.UTF8.GetBytes(Sb.ToString())
        Request.ContentLength = dataArray.Length

        ' Write JSON content to request
        Dim RequestStream As Stream = Request.GetRequestStream()
        RequestStream.Write(dataArray, 0, dataArray.Length())

        ' Execute the request, returns response
        Dim Response As HttpWebResponse = Request.GetResponse()

        ' Load response into string builder
        Dim ResponseReader As StreamReader = New StreamReader(Response.GetResponseStream())
        Dim ResponseText As String = ResponseReader.ReadToEnd()
        Dim ResponseStringReader = New StringReader(ResponseText)

        ' Deserialise content from response string builder into instance of ZabbixResponse.
        Dim HostResponse As ZabbixResponse = Js.Deserialize(ResponseStringReader, GetType(ZabbixResponse))

        Return HostResponse
    End Function

#Region "Private Entities"
    Private Class RetrievedImage
        Public Property imageid As String
        Public Property imagetype As String
        Public Property name As String
        Public Property image As String
    End Class

    Private Class ZabbixResponse
        Public Property jsonrpc As String
        Public Property result As Object
        Public Property id As Nullable(Of Integer)
        Public Property [error] As ResponseError
    End Class

    Private Class ResponseError
        Public Property code As Integer
        Public Property message As String
        Public Property data As String
    End Class

    Private Class ZabbixRequest
        Public Property jsonrpc As String
        Public Property method As String
        Public Property params As Object
        Public Property id As Integer
        Public Property auth As String
    End Class

    Private Class AuthenticationParams
        Public Property user As String
        Public Property password As String
    End Class

    Private Class HostParams
        Public Property output As String()
        Public Property selectInterfaces As String()
        Public Property selectItems As String()
        Public Property templateids As String()
    End Class

    Private Class MapParams
        Public Property output As String
    End Class

    Private Class ImageParams
        Public Property output As String
    End Class

    Private Class Host
        Public Property hostid As Integer
        Public Property host As String
        Public Property interfaces As List(Of HostInterface)
        Public Property items As List(Of HostItem)
    End Class

    Private Class HostInterface
        Public Property interfaceid As String
        Public Property ip As String
    End Class

    Private Class HostItem
        Public Property itemid As String
        Public Property name As String
        Public Property value_type As Integer
        Public Property lastvalue As String
    End Class




    'Private Shadows Class Template
    '    Public Property templateid As String
    '    Public Property name As String
    'End Class

    'Private Class TemplateParams
    '    Public Property filter As FilterItems
    'End Class
#End Region
End Class
