Imports System.IO
Imports Newtonsoft.Json
Imports Newtonsoft.Json.Linq

''' <summary>
''' Uses a file cache store for reading data.
''' </summary>
''' <remarks>Requires the WebApiDAL for outputting the map.</remarks>
Public Class FileCacheDAL : Implements ZabbixDal

    Private _externalDal As ZabbixDal
    Private ImageCache As List(Of RetrievedImage) = Nothing

    Private ReadOnly Property OutputDAL As ZabbixDal
        Get
            If IsNothing(_externalDal) Then
                _externalDal = New WebApiDAL
            End If
            Return _externalDal
        End Get
    End Property

    Public Property FileCacheReadPath As String

    Private Function GetReadPath() As String
        If FileCacheReadPath <> "" Then
            Return FileCacheReadPath
        Else
            Dim RootPath As String = My.Application.Info.DirectoryPath

            Dim DirInfo As New DirectoryInfo(IO.Path.Combine(RootPath, "FileCache"))

            If DirInfo.Exists Then
                Dim SubDirs As DirectoryInfo() = DirInfo.GetDirectories     ' sub directories
                If SubDirs.Count > 0 Then
                    ' Get the last created folder (latest cache) if none specified
                    Return SubDirs.OrderBy(Function(x) x.CreationTime).ToArray(SubDirs.Count - 1).FullName
                End If
            End If

            Return ""
        End If
    End Function

    Public Sub Authenticate(UserName As String, Password As String) Implements ZabbixDal.Authenticate
        OutputDAL.Authenticate(UserName, Password)
    End Sub

    Public Sub CreateMap(Map As Map) Implements ZabbixDal.CreateMap
        OutputDAL.CreateMap(Map)
    End Sub

    Public Sub DeleteMap(SysMapId As Integer) Implements ZabbixDal.DeleteMap
        Try
            OutputDAL.DeleteMap(SysMapId)
        Catch ex As Exception
            ' Do nothing. If the map was deleted but not created again (maybe due to fault) then this 
            ' routine would throw an error and we want it to fail silently in this scenario.
        End Try

    End Sub

    Public Function IsAuthenticated() As Boolean Implements ZabbixDal.IsAuthenticated
        Return OutputDAL.IsAuthenticated
    End Function

    ''' <summary>
    ''' File Cache could access the file store map list, but as we will want to delete and rebuild the 'live' map it does not make sense.
    ''' </summary>
    ''' <returns></returns>
    Public Function GetMaps() As List(Of Map) Implements ZabbixDal.GetMaps
        Return OutputDAL.GetMaps()
    End Function

    Public Function GetMaps(ByVal FileCacheFolderPath As String) As List(Of Map)
        Dim DirInfo As New DirectoryInfo(FileCacheFolderPath)
        Dim FileInfo As FileInfo = Nothing
        If Not DirInfo.Exists Then
            Throw New ApplicationException("Requested File Cache location does not exist")
        End If

        For Each fi As FileInfo In DirInfo.EnumerateFiles
            If fi.Name = String.Concat("map", ".jarray") Then
                ' File found
                FileInfo = New FileInfo(fi.FullName)
            End If
        Next

        If IsNothing(FileInfo) Then
            Throw New ApplicationException("cache file for maps could not be found")
        Else
            Dim MapArray As JArray = JsonConvert.DeserializeObject(IO.File.ReadAllText(FileInfo.FullName))
            Return MapArray.ToObject(Of List(Of Map))
        End If
    End Function

    Public Function ImageId(ImageName As String) As Integer Implements ZabbixDal.ImageId
        Return ImageId(ImageName, GetReadPath)
    End Function

    Public Function ImageId(ImageName As String,
                            ByVal FileCacheFolderPath As String) As Integer
        Dim ret As Integer
        ret = GetImages(FileCacheFolderPath).Where(Function(x) x.name = ImageName).Select(Function(x) CInt(x.imageid)).FirstOrDefault()
        If ret = 0 Then
            ' Default if nothing found
            If GetImages(FileCacheFolderPath).Count > 0 Then
                ret = GetImages(FileCacheFolderPath)(0).imageid
            Else
                Throw New ApplicationException("No icons retrieved")
            End If
        End If
        Return ret
    End Function

    Public Function GetHosts() As List(Of HostDevice) Implements ZabbixDal.GetHosts
        Return GetHosts(GetReadPath)
    End Function

    Public Function GetHosts(ByVal FileCacheFolderPath As String) As List(Of HostDevice)
        Dim DirInfo As New DirectoryInfo(FileCacheFolderPath)
        Dim FileInfo As FileInfo = Nothing
        Dim HostJsonObjects As List(Of Host)
        If Not DirInfo.Exists Then
            Throw New ApplicationException("Requested File Cache location does not exist")
        End If

        ' Get the hosts file.
        For Each fi As FileInfo In DirInfo.EnumerateFiles
            If fi.Name = String.Concat("hosts", ".jarray") Then
                ' File found
                FileInfo = New FileInfo(fi.FullName)
            End If
        Next

        If IsNothing(FileInfo) Then
            Throw New ApplicationException("cache file for hosts could not be found")
        Else
            ' Read data out of hosts file.
            Dim HostJsonArray As JArray = JsonConvert.DeserializeObject(IO.File.ReadAllText(FileInfo.FullName))
            HostJsonObjects = HostJsonArray.ToObject(Of List(Of host))

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


                If Not IsNothing(H.items) Then
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
                End If
                If IsNothing(ret) Then
                    ret = New List(Of HostDevice)
                End If
                ret.Add(Hd)
            Next

            Return ret

        End If
    End Function

    Private Function GetImages(ByVal FileCacheFolderPath As String) As List(Of RetrievedImage)
        If IsNothing(ImageCache) Then
            Dim DirInfo As New DirectoryInfo(FileCacheFolderPath)
            Dim FileInfo As FileInfo = Nothing
            If Not DirInfo.Exists Then
                Throw New ApplicationException("Requested File Cache location does not exist")
            End If

            For Each fi As FileInfo In DirInfo.EnumerateFiles
                If fi.Name = String.Concat("images", ".jarray") Then
                    ' File found
                    FileInfo = New FileInfo(fi.FullName)
                End If
            Next

            If IsNothing(FileInfo) Then
                Throw New ApplicationException("cache file for images could not be found")
            Else
                Dim ImageArray As JArray = JsonConvert.DeserializeObject(IO.File.ReadAllText(FileInfo.FullName))
                ImageCache = ImageArray.ToObject(Of List(Of RetrievedImage))
                Return ImageCache
            End If
        Else
            Return ImageCache
        End If
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


#Region "Private Entities"
    Private Class RetrievedImage
        Public Property imageid As String
        Public Property imagetype As String
        Public Property name As String
        Public Property image As String
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

#End Region
End Class
