Imports System.Windows.Forms
Imports System.Windows.Ink
''' <summary>
''' Two Dimensional Rectangle Packing 
''' </summary>
Public Class RectanglePacking

    ''' <summary>
    ''' First Fit Decreasing Height algorith for rectangle packing. strip width defined with infinite number of strip permitted
    ''' </summary>
    ''' <param name="Rects"></param>
    ''' <param name="StripWidth">Width of the strips. if strip width is less than the width of the widest rectange an error will be thrown</param>
    ''' <returns>Always returns at least an empty list. Never returs nothing</returns>
    Public Shared Function FFDH(ByVal Rects As List(Of Rect),
                ByVal StripWidth As Double) As List(Of Rect)

        ' For a basic explanation of the algorithm see E.G. Coffman Jr. and M.R. Garey and D.S. Johnson and R.E. Tarjan. 
        ' Performance bounds for level-oriented two-dimensional packing algorithms. SIAM Journal on Computing, 9:808--826, 1980.

        Dim MaxWidth As Double = 0D     ' Maximum width of all rectangles.
        Dim StripHeight As Double = 0D    ' Height of the current strip
        Dim StripY As Double = 0D           '  Y origin (bottom) of currect strip
        Dim CurrentX As Double = 0D         ' X position within current strip
        Dim I As Integer                    ' Loop memory
        Dim ret As New List(Of Rect)
        Dim AssignedRet As Rect
        If Not IsNothing(Rects) AndAlso Rects.Count > 0 Then
            MaxWidth = Rects.Max(Function(r) r.Width)       ' Rectangle with maximum width
            If MaxWidth > StripWidth Then
                Throw New ArgumentOutOfRangeException("Strip width cannot be less than the width of the widest rectangle.")
            End If
            ' Sort rectangles in input list by height descending
            Rects.Sort(New CompareHeight)
            Rects.Reverse()

            Do While Rects.Count > 0
                ' Loop until all rectangles are assigned.
                ' current strip is the height of the tallest rectange. Rectangles are ordered by height and assigned rectangles are deleted from the 
                ' input variable, so the first rectangle fits this requirement
                StripHeight = Rects(0).Height
                CurrentX = 0
                I = 0
                Do
                    If StripWidth - CurrentX >= Rects(I).Width Then
                        ' Available width is more than the width of the current rectangle
                        ' Create a new rectange object and put it in the return variable
                        AssignedRet = New Rect
                        With Rects(I)
                            AssignedRet.Id = .Id
                            AssignedRet.Width = .Width
                            AssignedRet.Height = .Height
                            AssignedRet.X = CurrentX
                            AssignedRet.Y = StripY
                        End With
                        ret.Add(AssignedRet)
                        CurrentX += AssignedRet.Width       ' Move the x position within the strip
                        ' Remove the assigned rectangle from the input variable
                        Rects.RemoveAt(I)
                    Else
                        I += 1      ' Itterate
                    End If
                Loop While I < Rects.Count
                StripY += StripHeight       ' Next strip Y position is current strip's Y plus current strips height.
            Loop
        End If
        Return ret
    End Function

    Public Class Rect : Implements IComparable(Of Rect)

        Public Property Width As Double
        Public Property Height As Double
        Public Property Id As Integer
        Public Property X As Double
        Public Property Y As Double

        ''' <summary>
        ''' Default compare is by ID
        ''' </summary>
        ''' <param name="other"></param>
        ''' <returns></returns>
        Public Function CompareTo(other As Rect) As Integer Implements IComparable(Of Rect).CompareTo
            Return Id.CompareTo(other.Id)
        End Function
    End Class

    ''' <summary>
    ''' Compare rectanges by height
    ''' </summary>
    Public Class CompareHeight : Inherits Comparer(Of Rect)
        Public Overrides Function Compare(x As Rect, y As Rect) As Integer
            ' x > y return 1
            ' x = y return 0
            ' x < y return -1
            If IsNothing(x) Then
                Return -1
            ElseIf IsNothing(y) Then
                Return 1
            Else
                Return x.Height.CompareTo(y.Height)
            End If
        End Function
    End Class
End Class
