''' <summary>
''' IP range defined by upper a lower range limits
''' </summary>
Public Class IpRange
    Public Property IpUpper As UInt32
    Public Property IpLower As UInt32
    Public Sub New(ByVal Upper As UInt32, ByVal Lower As UInt32)
        IpUpper = Upper
        IpLower = Lower
    End Sub
    Public Sub New()

    End Sub
End Class
