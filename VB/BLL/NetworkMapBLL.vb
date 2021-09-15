Imports System.Text
Imports ZabbixTopology.Common
Imports ZabbixTopology.Common.Enums

Public Class NetworkMapBLL
    'Public Enum MapLayout
    '    Flat
    '    LinearTree
    'End Enum

    Public Enum DataSourceType
        WebApi
        FileStore
    End Enum

    ''' <summary>
    ''' Data Access Layer
    ''' </summary>
    Private DAL As ZabbixDal = Nothing
    Private Property _dataSource As DataSourceType = DataSourceType.WebApi
    Private Property _fileCacheReadPath As String

    Public Property DataSource As DataSourceType
        Get
            Return _dataSource
        End Get
        Set(value As DataSourceType)
            _dataSource = value
            ApplyDataSource()
        End Set
    End Property

    ''' <summary>
    ''' Where file cache type DALs can READ data from. Should not affect the WRITE data path of the DAL.
    ''' </summary>
    ''' <returns></returns>
    Public Property FileCacheReadPath As String
        Get
            Return _fileCacheReadPath
        End Get
        Set(value As String)
            _fileCacheReadPath = value
            If DataSource = DataSourceType.FileStore Then
                CType(DAL, FileCacheDAL).FileCacheReadPath = FileCacheReadPath
            End If
        End Set
    End Property

    Public Sub New()
        ' Change Data access layer here.
        ApplyDataSource()
    End Sub

    ''' <summary>
    ''' Change to the selected data source. 
    ''' </summary>
    Private Sub ApplyDataSource()
        DAL = Nothing

        Select Case DataSource
            Case DataSourceType.FileStore
                DAL = New FileCacheDAL
                CType(DAL, FileCacheDAL).FileCacheReadPath = FileCacheReadPath
            Case DataSourceType.WebApi
                DAL = New WebApiDAL
        End Select
    End Sub

    Public Function IsAuthenticated() As Boolean
        Return DAL.IsAuthenticated
    End Function

    Public Sub Authenticate(ByVal UserName As String,
                     ByVal Password As String)
        DAL.Authenticate(UserName, Password)
    End Sub

    Public Function GetMaps() As List(Of Map)
        Return DAL.GetMaps()
    End Function

    Public Sub CreateMap(Map As Map)
        DAL.CreateMap(Map)
    End Sub

    Sub DeleteMap(SysMapId As Integer)
        DAL.DeleteMap(SysMapId)
    End Sub

    Public Function GetHosts() As List(Of HostDevice)
        Return DAL.GetHosts()
    End Function

    ''' <summary>
    ''' Retrieve hosts that are within the given IP range.
    ''' </summary>
    ''' <param name="IpRanges"></param>
    ''' <returns></returns>
    Public Function GetHosts(ByVal IpRanges As List(Of IpRange)) As List(Of HostDevice)
        Dim HostList As List(Of HostDevice) = DAL.GetHosts
        Dim i As Integer
        Dim Matched As Boolean = False
        If Not IsNothing(HostList) AndAlso HostList.Count > 0 Then
            For i = HostList.Count - 1 To 0 Step -1
                Matched = False   ' Reset matched flag
                For Each IpAddress As String In HostList(i).IpAddresses
                    ' Itterate the IP addresses assigned to the host. If any are within the desired filter range then step on to the next host
                    If IpUtil.IpAddressInRanges(IpAddress, IpRanges) Then
                        ' Match found, so leave this host in place.
                        Matched = True
                    End If
                Next

                If Not Matched Then
                    ' none of the host IP addresses were within the desired range, so remove it.
                    HostList.RemoveAt(i)
                End If
            Next
        End If
        Return HostList
    End Function

    ''' <summary>
    ''' Collects the host information and then produces a host map which is then pushed to the Zabbix server.
    ''' </summary>
    ''' <param name="NodeXSpace">Interspacing between nodes in the horizontal direction</param>
    ''' <param name="NodeYSpace">Interspacing between nodes in the vertical direction</param>
    ''' <param name="TreePadding">Margin array starting from 12 O'clock position and working clockwise.</param>
    Public Sub CreateHostMap(ByVal DisplayName As String,
                             ByVal Width As Integer,
                             ByVal Height As Integer,
                             ByVal HubImageName As String,
                             ByVal SwitchImageName As String,
                             ByVal UnknownImageName As String,
                             ByVal SortingMethod As SortingMethod,
                             Optional CreatePseudoHosts As Boolean = True,
                             Optional CreatePseudoHubs As Boolean = True,
                             Optional IpFilter As String = "",
                             Optional NodeXSpace As Double = 100.0,
                             Optional NodeYSpace As Double = 100.0,
                             Optional TreePadding() As Integer = Nothing,
                             Optional IncludeLinkLabels As Boolean = True)

        Dim Map As Map
        Dim IpRanges As List(Of IpRange) = Nothing       ' IP ranges used to filter acceptable hosts.


        If IpFilter <> "" Then
            IpRanges = New List(Of IpRange)
            IpUtil.TryParseIpRanges(IpFilter, IpRanges)
        End If

        Dim HostDevices As List(Of HostDevice) = If(IsNothing(IpRanges), GetHosts(), GetHosts(IpRanges))

        Dim HostLinks As List(Of HostLink) = HostDevicesLinkTable(HostDevices)

        ' Add hosts that are detected on remote ports only
        If CreatePseudoHosts Then
            AddPseudoHosts(HostDevices, HostLinks)
        End If

        If CreatePseudoHubs Then
            ' Create hubs to represent multiple connections to a single port on a host.
            AddPseudoHubs(HostDevices, HostLinks)
        End If

        If HostDevices.Count = 0 Then
            ' Nothing to create.
            Throw New ApplicationException("No hosts. Cannot create blank map")
        End If

        Map = New Map
        With Map
            .width = Width
            .height = Height
            .name = DisplayName
        End With

        ' Place all items and links on a map
        PopulateMap(HostDevices,
                        HostLinks,
                        Map,
                        HubImageName,
                        SwitchImageName,
                        UnknownImageName,
                        IncludeLinkLabels)

        ' Define Topology mapper parameters
        Dim SortingStrategy As New List(Of SortingMethod)
        SortingStrategy.Add(SortingMethod)

        ' Call topology mapper
        ZabbixTopology.MapLayout.LayoutMap(Map, NodeXSpace, NodeYSpace, TreePadding, SortingStrategy)

        ' Send the map to Zabbix server.
        CreateMap(Map)

    End Sub

    ''' <summary>
    ''' Create a map with links from a list of host devices.
    ''' </summary>
    Public Sub PopulateMap(ByVal HostDevices As List(Of HostDevice),
                            ByVal HostLinks As List(Of HostLink),
                            ByRef Map As Map,
                           ByVal HubImageName As String,
                           ByVal SwitchImageName As String,
                           ByVal UnknownImageName As String,
                           ByVal IncludeLinkLabels As Boolean)

        Dim Selements As List(Of Selement) = Nothing
        Dim Selement As Selement = Nothing
        Dim MapCreateElement As MapCreateElement = Nothing
        Dim SElementId As Integer = 0
        Dim PseudoHosts As Boolean = True            ' If devices are found on remote ports should we create pseudo hosts so that they are included in the map output?
        Dim HubImageId As Integer = DAL.ImageId(HubImageName)
        Dim SwitchImageId As Integer = DAL.ImageId(SwitchImageName)
        Dim UnknownImageId As Integer = DAL.ImageId(UnknownImageName)

        If IsNothing(HostDevices) Then
            Exit Sub
        End If

        For Each Host As HostDevice In HostDevices
            ' Add all of the hosts to the map
            MapCreateElement = New MapCreateElement
            MapCreateElement.hostid = Host.HostId
            SElementId += 1
            Selement = New Selement
            With Selement
                .elements = New List(Of MapCreateElement)
                .elements.Add(MapCreateElement)
                .selementid = SElementId
                If Host.HostId = 0 Then
                    ' This is a pseudo host and so doesn't really exist in Zabbix.
                    .elementtype = 4        ' Image
                    If Host.HostName = "Pseudo hub" Then
                        .iconid_off = HubImageId
                    Else
                        .iconid_off = UnknownImageId
                    End If

                    If Not IsNothing(Host.ChassisId) Then
                        .label = Host.ChassisId.ToString
                    End If
                Else
                    ' This is a true host.
                    .elementtype = 0        ' Host
                    .iconid_off = SwitchImageId     ' Must be declared if automatic icon mapping is not enabled 
                    .label = Host.HostName
                End If
            End With

            ' Record the SElementId against the appropriate link items so that we can create the selement links in a fast loop.
            If Not IsNothing(HostLinks) AndAlso HostLinks.Count > 0 Then
                For i As Integer = 0 To HostLinks.Count - 1
                    If HostLinks(i).A.HostDeviceId = Host.HostDeviceId Then
                        HostLinks(i).A.MapSElementID = SElementId
                    ElseIf HostLinks(i).B.HostDeviceId = Host.HostDeviceId Then
                        HostLinks(i).B.MapSElementID = SElementId
                    End If
                Next
            End If

            If IsNothing(Selements) Then
                Selements = New List(Of Selement)
            End If
            Selements.Add(Selement)
        Next

        If IsNothing(Map) Then
            Map = New Map
        End If

        Map.selements = Selements

        ' Create links between map elements
        Map.links = GetMapElementLinks(HostDevices, HostLinks, IncludeLinkLabels)

    End Sub

    ''' <summary>
    ''' Get links that join map elements
    ''' </summary>
    ''' <param name="HostDevices">Collection of Host Objects</param>
    ''' <param name="HostLinks">Collection of Links between Host objects</param>
    ''' <param name="IncludeLabels">Should we render labels on the element links</param>
    ''' <returns></returns>
    Private Function GetMapElementLinks(ByVal HostDevices As List(Of HostDevice),
                                       ByVal HostLinks As List(Of HostLink),
                                       Optional ByVal IncludeLabels As Boolean = True) As List(Of SelementLink)

        Dim Link As SelementLink = Nothing
        Dim Links As List(Of SelementLink) = Nothing
        Dim sb As StringBuilder

        If IsNothing(HostLinks) OrElse HostLinks.Count = 0 Then
            Return Nothing
        End If

        For Each HostLink As HostLink In HostLinks
            If HostLink.A.MapSElementID <> 0 And HostLink.B.MapSElementID <> 0 Then
                Link = New SelementLink

                With Link
                    .selementid1 = HostLink.A.MapSElementID
                    .selementid2 = HostLink.B.MapSElementID

                    If IncludeLabels Then
                        ' Build the label for the link
                        sb = New StringBuilder

                        ' Left side host name
                        If HostLink.A.HostDeviceId > 0 Then
                            For Each Host As HostDevice In HostDevices
                                If Host.HostId = HostLink.A.HostDeviceId Then
                                    sb.Append(Host.HostName)
                                End If
                            Next
                        Else
                            sb.Append(HostLink.A.HostReference)
                        End If

                        sb.Append(" [")
                        sb.Append(HostLink.A.PortReference)
                        sb.Append("]")
                        sb.Append(vbCrLf)

                        ' Right side host name
                        If HostLink.B.HostDeviceId > 0 Then
                            For Each Host As HostDevice In HostDevices
                                If Host.HostId = HostLink.B.HostDeviceId Then
                                    sb.Append(Host.HostName)
                                End If
                            Next
                        Else
                            sb.Append(HostLink.B.HostReference)
                        End If

                        sb.Append(" [")
                        sb.Append(HostLink.B.PortReference)
                        sb.Append("]")

                        .label = sb.ToString()
                    End If
                End With
                If IsNothing(Links) Then
                    Links = New List(Of SelementLink)
                End If
                Links.Add(Link)
            End If
        Next
        Return Links
    End Function

    ''' <summary>
    ''' Get a free Device Host ID so that I can add new hosts (e.g. pseudo hosts) without creating ID conflicts
    ''' </summary>
    ''' <param name="HostDevices"></param>
    ''' <returns></returns>
    Private Function FreeDeviceHostId(ByVal HostDevices As List(Of HostDevice))
        Dim I As Integer = 0
        Dim Conflict As Boolean

        Do
            I += 1
            Conflict = False
            For Each HD As HostDevice In HostDevices
                If HD.HostDeviceId = I Then
                    ' We have found an existing host already using this host device ID.
                    Conflict = True
                End If
            Next
        Loop While Conflict = True

        Return I
    End Function

    ''' <summary>
    ''' Identify locations where multiple devices are detected connected to a single port and insert a hub in this place.
    ''' This is a visual clue only because such conditions can exist without hubs, such as with virtual machines, which respond together
    ''' from the host (plus the host too).
    ''' </summary>
    ''' <param name="HostDevices"></param>
    ''' <param name="HostLinks"></param>
    Public Sub AddPseudoHubs(ByRef HostDevices As List(Of HostDevice),
                             ByRef HostLinks As List(Of HostLink))

        Dim HubsRequired As New List(Of HostLink)
        Dim Hub As List(Of HostLink)
        Dim Hubs As List(Of List(Of HostLink)) = Nothing
        Dim HubMatched As Boolean
        Dim NewHub As HostDevice = Nothing
        Dim NewHubHostLink As HostLink
        Dim NewHubHostLinks As List(Of HostLink)
        Dim OuterCounter As Integer                 ' external itteration loop for moving links
        Dim InnerCounter As Integer                 ' internal itteration loop for moving links
        Dim ExistingHubMember As HostLink

        If IsNothing(HostDevices) OrElse HostDevices.Count = 0 Then
            ' Nothing to do
            Exit Sub
        End If

        If IsNothing(HostLinks) OrElse HostLinks.Count < 2 Then
            ' Also nothing to do
            Exit Sub
        End If

        OuterCounter = (HostLinks.Count * 2) - 1        ' If four host links then this equals 7.
        InnerCounter = OuterCounter - 1                 ' If four host links then this equals 6.

        ' Identify host links that should be connected through a hub, remove them from the current HostLinks object and 
        ' add them to a HubsRequired object
        ' Scan backwards through the range taking twice the HostLink count because we have to scan both the left and right
        ' side of the HostLink.
        Do
            ' Outer loop
            Do
                ' Inner loop
                If InnerCounter <> OuterCounter Then
                    If InnerCounter < HostLinks.Count AndAlso OuterCounter < HostLinks.Count Then
                        ' Evaluating left (A) hand side of inner against left (A) hand side of outer
                        If HostLinks(InnerCounter).A.IsEqual(HostLinks(OuterCounter).A) Then
                            ' Match detected
                            ' The outer counter should always be more than the inner counter
                            HubsRequired.Add(HostLinks(InnerCounter))
                            HubsRequired.Add(HostLinks(OuterCounter))
                        End If
                    ElseIf InnerCounter < HostLinks.Count Then
                        ' Evaluating left hand side of inner against right hand side of outer
                        If OuterCounter - HostLinks.Count <> InnerCounter Then
                            ' Ensure we are not evaluating left (A) and right (B) of the same hostlink.
                            If HostLinks(InnerCounter).A.IsEqual(HostLinks(OuterCounter - HostLinks.Count).B) Then
                                HubsRequired.Add(HostLinks(InnerCounter))
                                HubsRequired.Add(HostLinks(OuterCounter - HostLinks.Count))
                            End If
                        End If
                    ElseIf OuterCounter < HostLinks.Count Then
                        ' Evaluating right hand side of inner against Left hand side of outer
                        ' the inner counter should always be less than the outer counter. If the outer counter is low enough to be on the left hand side
                        ' then the inner counter must also be on the left hand side. 
                        Throw New ApplicationException("Assumed to be impossible.")
                    Else
                        ' Evaluating right hand side (B) of inner against right hand side (B) of outer
                        If HostLinks(InnerCounter - HostLinks.Count).B.IsEqual(HostLinks(OuterCounter - HostLinks.Count).B) Then
                            HubsRequired.Add(HostLinks(InnerCounter - HostLinks.Count))
                            HubsRequired.Add(HostLinks(OuterCounter - HostLinks.Count))
                        End If
                    End If
                End If
                If OuterCounter > (HostLinks.Count * 2) - 1 Then
                    ' We have removed host links and now the outer count is out of range
                    Exit Do
                End If
                If InnerCounter = 0 Then
                    Exit Do
                End If
                ' Decrement
                InnerCounter -= 1
            Loop
            If OuterCounter > (HostLinks.Count * 2) - 1 Then
                ' We must have removed some host links and now the outer counter is out of range
                OuterCounter = (HostLinks.Count * 2) - 1
            ElseIf OuterCounter < 2 Then
                ' OuterCounter = 1 will have been compared against InnerCounter = 0, so no need to test OuterCounter = 0.
                Exit Do
            Else
                ' Decrement
                OuterCounter -= 1
            End If
            InnerCounter = OuterCounter - 1
        Loop

        ' Remove Duplicate HubsRequired
        If HubsRequired.Count > 1 Then
            For OuterCounter = HubsRequired.Count - 1 To 0 Step -1
                If OuterCounter = 0 Then Exit For
                For InnerCounter = OuterCounter - 1 To 0 Step -1
                    If HubsRequired(OuterCounter).A.IsEqual(HubsRequired(InnerCounter).A) And
                            HubsRequired(OuterCounter).B.IsEqual(HubsRequired(InnerCounter).B) Then
                        ' Duplicate found
                        HubsRequired.RemoveAt(OuterCounter)
                        Exit For
                    End If
                Next
            Next
        End If

        ' Delete from HostLinks any links that have been transfered to HubsRequired
        If HostLinks.Count > 0 And HubsRequired.Count > 0 Then
            For OuterCounter = HostLinks.Count - 1 To 0 Step -1
                For InnerCounter = 0 To HubsRequired.Count - 1
                    If HostLinks(OuterCounter).A.IsEqual(HubsRequired(InnerCounter).A) And
                            HostLinks(OuterCounter).B.IsEqual(HubsRequired(InnerCounter).B) Then
                        HostLinks.RemoveAt(OuterCounter)
                        Exit For
                    End If
                Next
            Next
        End If

        ' HostsLinks should have been stripped of all 'hubbable' host links.
        ' HubsRequired contains all links that need to connect to hubs

        ' Group the host links against the required hubs. Neccessary where more than one hub is required.
        If HubsRequired.Count > 0 Then
            ' We need some hubs
            ' We now need to group the results so that we can work out how many hubs need to be created. 
            Hubs = New List(Of List(Of HostLink))
            For Each RequiredHub As HostLink In HubsRequired
                With RequiredHub
                    If .A.IsBlank() Or .B.IsBlank() Then
                        Throw New ApplicationException("AddPseudoHubs required hub linking has detected invalid data.")
                    End If
                End With

                If Hubs.Count = 0 Then
                    Hub = New List(Of HostLink)
                    Hub.Add(RequiredHub)
                    Hubs.Add(Hub)
                Else
                    ' Check to see if hub already exists and add to that. Otherwise create a new hub
                    HubMatched = False
                    For Each ExistingHub As List(Of HostLink) In Hubs
                        For i As Integer = ExistingHub.Count - 1 To 0 Step -1
                            ExistingHubMember = ExistingHub(i)
                            ' If either side of the requiredhub matches either side of the existing hub member, then this requiredhub hostlink should go into this hub
                            ' if existing left = new left or existing left = new right or existing right = new left or existing right = new right.
                            If ExistingHubMember.A.IsEqual(RequiredHub.A) Or
                                    ExistingHubMember.A.IsEqual(RequiredHub.B) Or
                                    ExistingHubMember.B.IsEqual(RequiredHub.A) Or
                                    ExistingHubMember.B.IsEqual(RequiredHub.B) Then
                                ExistingHub.Add(RequiredHub)
                                HubMatched = True
                            End If
                        Next
                    Next
                    If Not (HubMatched) Then
                        ' Add new hub
                        Hub = New List(Of HostLink)
                        Hub.Add(RequiredHub)
                        Hubs.Add(Hub)
                    End If
                End If
            Next
        End If

        ' Add hubs and their associated links
        If Not IsNothing(Hubs) AndAlso Hubs.Count > 0 Then
            For Each RequiredHub As List(Of HostLink) In Hubs
                If RequiredHub.Count > 1 Then
                    ' Cannot produce a hub for only one link. It doesn't make sense.
                    NewHubHostLinks = New List(Of HostLink)
                    NewHub = New HostDevice
                    NewHub.HostDeviceId = FreeDeviceHostId(HostDevices)
                    NewHub.HostName = "Pseudo hub"
                    HostDevices.Add(NewHub)
                    For Each RequiredLink As HostLink In RequiredHub
                        If NewHubHostLinks.Where(Function(hl) hl.B.HostDeviceId = RequiredLink.A.HostDeviceId).Count() = 0 Then
                            ' Required link left host does not exist in the list of new hub links. 
                            ' New hub host links will put the hub as the left host id.
                            NewHubHostLink = New HostLink
                            With NewHubHostLink
                                .A.HostDeviceId = NewHub.HostDeviceId
                                .A.HostReference = "Pseudo hub"
                                .B.HostDeviceId = RequiredLink.A.HostDeviceId
                                .B.HostReference = RequiredLink.A.HostReference
                                .B.PortReference = RequiredLink.A.PortReference
                            End With
                            NewHubHostLinks.Add(NewHubHostLink)
                        End If
                        ' now do the right side.
                        If NewHubHostLinks.Where(Function(hl) hl.B.HostDeviceId = RequiredLink.B.HostDeviceId).Count() = 0 Then
                            ' Required link left host does not exist in the list of new hub links. 
                            ' New hub host links will put the hub as the left host id.
                            NewHubHostLink = New HostLink
                            With NewHubHostLink
                                .A.HostDeviceId = NewHub.HostDeviceId
                                .A.HostReference = "Pseudo hub"
                                .B.HostDeviceId = RequiredLink.B.HostDeviceId
                                .B.HostReference = RequiredLink.B.HostReference
                                .B.PortReference = RequiredLink.B.PortReference
                            End With
                            NewHubHostLinks.Add(NewHubHostLink)
                        End If
                    Next
                    If NewHubHostLinks.Count > 0 Then
                        ' Copy new host links into the main host links list
                        For Each NewLink As HostLink In NewHubHostLinks
                            HostLinks.Add(NewLink)
                        Next
                    End If
                End If
            Next
        End If
    End Sub

    ''' <summary>
    ''' Identify remote devices that have not been identified as hosts. This occurs where a switch is reporting devices by LLDP that the scanner
    ''' did not find via SNMP or other methods. 
    ''' </summary>
    ''' <param name="HostDevices"></param>
    ''' <param name="HostLinks"></param>
    Public Sub AddPseudoHosts(ByRef HostDevices As List(Of HostDevice),
                             ByRef HostLinks As List(Of HostLink))

        Dim NewHostDevice As HostDevice
        Dim PseudoHosts As New List(Of HostDevice)
        Dim NewPseudoMatched As Boolean                 ' Have we managed to match the desired pseudo host to one already created earlier in the process.

        If (IsNothing(HostDevices) Or IsNothing(HostLinks)) OrElse (HostDevices.Count = 0 Or HostLinks.Count = 0) Then
            ' Nothing to do
            Exit Sub
        End If

        ' Create host devices with HostID as zero but HostDeviceId as a unique value.
        For Each HL As HostLink In HostLinks
            If HL.A.HostDeviceId = 0 And HL.B.HostDeviceId = 0 Then
                Throw New ApplicationException("Both left and right sides of the link have no host ID")
            End If
            If HL.A.HostDeviceId = 0 Or HL.B.HostDeviceId = 0 Then
                NewPseudoMatched = False
                ' Check to see if the pseudo host has already been added. 
                For Each PHost As HostDevice In PseudoHosts
                    If PHost.ChassisId = If(HL.A.HostDeviceId = 0, HL.A.HostReference, HL.B.HostReference) Then
                        ' pseudo host already exists, just link to this one.
                        If HL.A.HostDeviceId = 0 Then
                            HL.A.HostDeviceId = PHost.HostDeviceId
                        Else
                            HL.B.HostDeviceId = PHost.HostDeviceId
                        End If
                        NewPseudoMatched = True
                    End If
                Next
                If Not NewPseudoMatched Then
                    ' The desired psuedo host has not already been generated, so add it as a new one here. 
                    NewHostDevice = New HostDevice
                    With NewHostDevice
                        .ChassisId = If(HL.A.HostDeviceId = 0, HL.A.HostReference, HL.B.HostReference)
                        .HostDeviceId = FreeDeviceHostId(HostDevices)

                        ' Update the host links record with the newly create HostDeviceId. 
                        If HL.A.HostDeviceId = 0 Then
                            HL.A.HostDeviceId = .HostDeviceId
                        Else
                            HL.B.HostDeviceId = .HostDeviceId
                        End If
                    End With

                    ' Add it in both places so that we can keep track of it. 
                    PseudoHosts.Add(NewHostDevice)
                    HostDevices.Add(NewHostDevice)
                End If
            End If
        Next
    End Sub

    ''' <summary>
    ''' Create a table showing the interconnections between hosts. 
    ''' </summary>
    ''' <param name="HostDevices"></param>
    ''' <returns>Index reference, not object references</returns>
    Public Function HostDevicesLinkTable(ByRef HostDevices As List(Of HostDevice)) As List(Of HostLink)
        If IsNothing(HostDevices) Then
            Return Nothing
        End If

        Dim Link As HostLink
        Dim ret As List(Of HostLink) = Nothing

        For Each hd As HostDevice In HostDevices
            If Not IsNothing(hd.RemoteDevices) Then
                For Each rd As RemoteDevice In hd.RemoteDevices
                    If rd.ChassisId = "" And rd.Host = "" And rd.PortId = "" Then
                        ' there is not enough information for us to make a connection.
                        Continue For
                    End If

                    Link = New HostLink
                    ' Link information will be populated without matching remote host ID.
                    With Link
                        .A.HostDeviceId = hd.HostDeviceId
                        .A.HostReference = hd.ChassisId
                        .A.PortReference = rd.LocalPortName
                        .B.HostReference = rd.ChassisId
                        .B.PortReference = rd.PortId
                    End With

                    ' Discover right hand (B side) host ID
                    For Each RightHd As HostDevice In HostDevices
                        If StripSeparators(RightHd.ChassisId) = StripSeparators(rd.ChassisId) Then
                            ' This is the host device references in the right host reference
                            Link.B.HostDeviceId = RightHd.HostDeviceId
                        End If
                    Next

                    If IsNothing(ret) Then
                        ret = New List(Of HostLink)
                    End If
                    ret.Add(Link)
                Next
            End If
        Next

        If Not IsNothing(ret) AndAlso ret.Count > 0 Then
            ' Remove duplicates
            Dim I As Integer
            For I = 0 To ret.Count - 1
                If I > ret.Count - 1 Then
                    ' the target count in the For...Next loop for 'I' does not update each itteration, so deletions can cause 'I' to overflow.
                    Exit For
                End If
                For J = ret.Count - 1 To 0 Step -1
                    If I <> J Then
                        If ret(I).A.HostDeviceId = ret(J).B.HostDeviceId And ret(I).B.HostDeviceId = ret(J).A.HostDeviceId Then
                            ret.RemoveAt(J)
                        End If
                    End If
                Next
            Next
        End If

        Return ret
    End Function

    ''' <summary>
    ''' Remove separators from strings such as blank spaces, colons, hyphens etc.
    ''' </summary>
    ''' <param name="InputString"></param>
    ''' <returns></returns>
    Private Function StripSeparators(ByVal InputString As String) As String
        If IsNothing(InputString) Then Return InputString
        Return InputString.Replace(" ", "").Replace(":", "").Replace("-", "")
    End Function
End Class
