
''' <summary>
''' a single port local to a switch or device
''' </summary>
Public Class LocalPort
    Private Property _parentHost As HostDevice
    Public ReadOnly Property ParentHost As HostDevice
        Get
            Return _parentHost
        End Get
    End Property
    Public Property LocalPortName As String         ' Name identifing the port within the host.
    Public Property InterfaceId As String           ' Address or identifier of the local interface / port
    Public Property InterfaceIdType As Common.Enums.PortIdType      ' What type of ID have we got above (e.g. 4= MAC address, see LLDP MIB).
    Public Property RemotePorts As List(Of RemotePort)        ' List of devices detected connected to the port. This would be LLDP-MIB::lldpRemoteSystemsData in SNMP. There could be more than one connected device if there is no smart device directly connected, such as a hub connected to the port with multiple devices connected to that.
    Public Sub New(HostDevice As HostDevice)
        _parentHost = HostDevice
    End Sub
End Class
