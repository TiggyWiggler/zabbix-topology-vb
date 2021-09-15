''' <summary>
''' A collection of unconnected trees (in Zabbix Parlance this is a map, even though the map could be a single tree)
''' </summary>
Public Class Forest
    Public Property Trees As New List(Of Tree)

    ''' <summary>
    ''' Arrange the trees within the forest space
    ''' </summary>
    ''' <remarks>SetAbsolutes() must be called on the tree objects before calling this method</remarks>
    Public Sub ArrangeTrees()
        ' Create a list of generalised rectangles representing the individual trees which can be passed to a rectangle packing algorithm.
        Dim R As RectanglePacking.Rect
        Dim Rects As New List(Of RectanglePacking.Rect)
        Dim I As Integer = 0        ' Loop counter  
        Dim TotalArea As Double = 0D        ' Total area of all rectangles
        Dim TargetWidth As Double = 0D      ' Target width for the output rectangle
        Dim MaxWidth As Double = 0D           ' Maxmimum width of all rectangles
        For Each T As Tree In Trees
            R = New RectanglePacking.Rect
            R.Id = I        ' The rectangle ID will represent the ordinal position of the tree within the trees list.
            I += 1           ' Itterate I
            R.Width = T.Width
            R.Height = T.Height
            Rects.Add(R)
            TotalArea += R.Width * R.Height
            If R.Width > MaxWidth Then
                ' This is the widest rectangle so far. Store the result
                MaxWidth = R.Width
            End If
        Next

        ' We are ideally looking for an aspect ratio close to 1.0. To make this simple we will pass a strip width.
        ' Later implementations can itterate variables to find the solution that is closest to 1.0. For now this will do.
        TargetWidth = Math.Sqrt(TotalArea)

        If TargetWidth < MaxWidth Then
            ' Cannot have a target width smaller than the maximum width of any single rectangle.
            TargetWidth = MaxWidth
        End If
        If TargetWidth = 0 Then
            Throw New ApplicationException("Attempt to pack containing rectange of zero width.")
        End If

        ' Perform the rectangle packing
        Rects = RectanglePacking.FFDH(Rects, TargetWidth)

        ' Copy the position data from the generalised and packed rectangle collection to the collection of trees. Essentially positioning the trees according
        ' to the output of the rectangle packing algorithm.
        For Each R In Rects
            Trees(R.Id).AbsoluteX = R.X
            Trees(R.Id).AbsoluteY = R.Y
        Next

    End Sub

    ''' <summary>
    ''' Find an tree node by the ElementId (unique ID)
    ''' </summary>
    ''' <param name="ElementId"></param>
    ''' <returns></returns>
    Public Function FindNodeByElementID(ByVal ElementId As Integer) As TreeNode
        Dim ret As TreeNode = Nothing
        For Each T As Tree In Trees
            ' Search through each tree itteratively for the desired treenode.
            ret = T.FindNodeByElementID(ElementId)
            If Not IsNothing(ret) Then
                ' we have the treenode. stop searching.
                Exit For
            End If
        Next
        Return ret
    End Function
End Class
