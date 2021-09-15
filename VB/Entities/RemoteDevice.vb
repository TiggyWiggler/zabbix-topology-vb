Public Class RemoteDevice
    Public Property MSAP As Integer                 ' Device Media Service Access Point for the local connection of the remote port.
    Public Property LocalPortName As String         ' Local port that the remote device is connected to. 
    Public Property ChassisId As String             ' Address or identifier of the connected device (e.g. MAC address)
    Public Property ChassisIdType As Common.Enums.ChassisIdType  ' What type of ID have we got above (e.g. 4= MAC address, see LLDP MIB).
    Public Property Host As String                  ' The connected device system name
    Public Property HostDescription As String       ' The connected device self issued description
    Public Property Description As String           ' Port Description
    Public Property PortId As String                    ' Address or identifier of the port on the remote connected Host that is connected to our local interface.
    Public Property PortIdType As Common.Enums.PortIdType            ' What sort of information is in the ID field.
End Class
