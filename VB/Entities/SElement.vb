''' <summary>
''' Special element used when creating or updating maps
''' </summary>
Public Class Selement
    Public Property selementid As String
    Public Property elementtype As Integer
    Public Property iconid_off As String
    Public Property elements As List(Of MapCreateElement)
    Public Property x As Integer
    Public Property y As Integer
    Public Property label As String
End Class
