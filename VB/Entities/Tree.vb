' A tree which is a collection of connected (linked) nodes
Public Class Tree
    Public Property Nodes As New List(Of TreeNode)

    ''' <summary>
    ''' Absolute position of the tree  on the X axis. 
    ''' </summary>
    ''' <returns></returns>
    Public Property AbsoluteX As Double = 0D

    ''' <summary>
    ''' Absolute position of the tree  on the Y axis. 
    ''' </summary>
    ''' <returns></returns>
    Public Property AbsoluteY As Double = 0D
    Public Property Width As Double = 0D
    Public Property Height As Double = 0D

    ''' <summary>
    ''' Find an tree node by the ElementId (unique ID)
    ''' </summary>
    ''' <param name="ElementId"></param>
    ''' <returns></returns>
    Public Function FindNodeByElementID(ByVal ElementId As Integer) As TreeNode
        Dim ret As TreeNode = Nothing
        If Not IsNothing(Nodes) AndAlso Nodes.Count > 0 Then
            ret = Nodes.FirstOrDefault(Function(n) n.ElementId = ElementId)
        End If
        Return ret
    End Function

    ''' <summary>
    ''' Get the root node of the tree. Returns nothing if no root node found.
    ''' </summary>
    ''' <returns></returns>
    Public Function GetRootNode() As TreeNode
        If Nodes.Count > 0 Then
            Return Nodes.Where(Function(n) n.Level = Nodes.Min(Function(mn) mn.Level)).FirstOrDefault()
        Else
            Return Nothing
        End If
    End Function

    ''' <summary>
    ''' Set relative positions for all nodes in the tree (not offsets or absolute positions). These are relative to the container tree
    ''' </summary>
    Public Sub SetNodesRelative()
        If Nodes.Count > 0 Then
            ' Get the root node by extracting the first treenode of the minimum level found in the tree's node collection.
            Dim Root As TreeNode = GetRootNode()

            If Not IsNothing(Root) Then
                Root.SetRelativePositions(0.0)
                Dim MinXPos As Double = Nodes.Min(Function(n) n.RelativeX)

                ' Move all nodes right so as to remove any negative absolute positions.
                For Each N As TreeNode In Nodes
                    N.RelativeX -= MinXPos
                Next
            Else
                Throw New ApplicationException("Root node could not be found")
            End If
        End If
    End Sub

    ''' <summary>
    ''' Set absolute positions for all nodes in the tree. These are relative to the forest.
    ''' </summary>
    Public Sub SetNodesAbsolute()
        If Nodes.Count > 0 Then
            For Each N As TreeNode In Nodes
                N.AbsoluteX = N.RelativeX + AbsoluteX
                N.AbsoluteY = N.RelativeY + AbsoluteY
            Next
        End If
    End Sub

    ''' <summary>
    ''' Calculate the height and width of the tree.
    ''' </summary>
    Public Sub CalculateExtents()
        If Nodes.Count > 0 Then
            ' Add 1 to the calculated dimension to account for the node at 0,0. 
            ' Without this step a tree with a single node would have width and height 0
            Width = (Nodes.Max(Function(n) n.RelativeX) - Nodes.Min(Function(n) n.RelativeX)) + 1
            Height = (Nodes.Max(Function(n) n.RelativeY) - Nodes.Min(Function(n) n.RelativeY)) + 1
        Else
            Width = 0D
            Height = 0D
        End If
    End Sub

    ''' <summary>
    ''' Returns a post-order traversal list of tree nodes
    ''' </summary>
    ''' <returns></returns>
    Public Function PostOrderList() As List(Of TreeNode)
        ' Great write up on the subject and sample code:
        ' https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/
        Dim Output As List(Of TreeNode) = Nothing
        Dim Root As TreeNode
        If Not IsNothing(Nodes) AndAlso Nodes.Count > 0 Then
            Output = New List(Of TreeNode)          ' Empty memory location
            Root = Nodes.Where(Function(n) n.Level = 0).FirstOrDefault      ' Find the root node

            If IsNothing(Root) Then
                Throw New ApplicationException("Root node could not be identified")
            End If

            Root.GetPostOrderList(Output)
        End If

        Return Output           ' Could be nothing.
    End Function


End Class
