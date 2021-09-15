
''' <summary>
''' A zabbix map 
''' </summary>
Public Class Map
    Public Property sysmapid As String
    Public Property name As String
    Public Property width As Integer
    Public Property height As Integer
    Public Property selements As List(Of Selement)
    Public Property links As List(Of SelementLink)
    Public Property label_type As Integer = 0
End Class
