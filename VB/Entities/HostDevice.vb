
Imports Newtonsoft.Json

Public Class HostDevice
    Public Property HostDeviceId As Integer ' Unique ID for this host device. Required if creating pseudo hosts as they have no host Id and we should not create host ids because they become serialised and could create false connections inside Zabbix
    Public Property HostId As Integer       ' Zabbix defined unique ID for each host.
    Public Property HostName As String      ' The device system name
    Public Property ChassisId As String
    Public Property ChassisType As Common.Enums.ChassisIdType
    ''' <summary>
    ''' Local ports on the switch of device
    ''' </summary>
    ''' <returns></returns>
    Public Property Ports As List(Of LocalPort)     ' Deprecated?
    Public Property RemoteDevices As List(Of RemoteDevice)
    Public Property IpAddresses As List(Of String)
End Class
