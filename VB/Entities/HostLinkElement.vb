''' <summary>
''' One side or element of a host link, a host link being the connection between two hosts, usually with specific ports stated.
''' </summary>
<Serializable()>
Public Class HostLinkElement
    <NonSerialized()> Private _MapSElementID As Integer
    Public Property HostDeviceId As Integer
    Public Property HostReference As String
    Public Property PortReference As String
    Public Property MapSElementID As Integer        ' During map element creation we need to remember which linked items related to which map elements for the s element link. Cannot use hosts as maps include items discovered through LLDP that are not hosts
        Get
            Return _MapSElementID
        End Get
        Set(value As Integer)
            _MapSElementID = value
        End Set
    End Property
    ''' <summary>
    ''' Is the given host link element equal to this one.
    ''' </summary>
    ''' <param name="ElementToCompare"></param>
    ''' <returns></returns>
    Public Function IsEqual(ByVal ElementToCompare As HostLinkElement)
        With ElementToCompare
            If Not IsBlank() AndAlso Not .IsBlank Then
                Return HostDeviceId = .HostDeviceId And
                        HostReference = .HostReference And
                        PortReference = .PortReference
            Else
                Return False
            End If
        End With
    End Function
    ''' <summary>
    ''' Is this host link element empty of data.
    ''' </summary>
    ''' <returns></returns>
    Public Function IsBlank()
        Return HostDeviceId = 0 And HostReference = "" And PortReference = ""
    End Function
End Class
