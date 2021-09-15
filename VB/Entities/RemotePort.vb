Public Class RemotePort
    Private Class RemotePort
        Public Property ChassisId As String             ' Address or identifier of the connected device (e.g. MAC address)
        Public Property ChassisIdType As Common.Enums.ChassisIdType  ' What type of ID have we got above (e.g. 4= MAC address, see LLDP MIB). 
        Public Property Host As String                  ' The connected device system name
        Public Property HostDescription As String       ' The connected device self issued description
        Public Property Description As String           ' Port Description
        Public Property Id As String                    ' Address or identifier of the port on the connected Host that is connected to out local interface.
        Public Property IdType As Common.Enums.PortIdType            ' What sort of information is in the ID field.
        Public Property PortObject As LocalPort    ' This is an object reference to the local port of the remote host once the mapping is created
    End Class
End Class
