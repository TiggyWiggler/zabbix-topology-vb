Imports System.Xml
''' <summary>
''' A single node or entity on a tree
''' </summary>
Public Class TreeNode : Implements IComparable(Of TreeNode)

    ''' <summary>
    ''' Which side of the countour do we need to scan.
    ''' </summary>
    Public Enum ContourSide
        Left
        Right
    End Enum

    ''' <summary>
    ''' Mapping to node from external source. See remarks
    ''' </summary>
    ''' <returns></returns>
    ''' <remarks>External source is the prepopulated map that was given to LayoutMap()</remarks>
    Public Property ElementId As Integer
    ''' <summary>
    ''' Depth from root. root is 0, undefined is -1
    ''' </summary>
    ''' <returns></returns>
    Public Property Level As Integer = -1
    ''' <summary>
    ''' Sort position according to parent. undefined is -1
    ''' </summary>
    ''' <returns></returns>
    Public Property SortPosition As Integer = -1
    Public Property LinkedNodes As New List(Of TreeNode)
    ''' <summary>
    ''' Horizontal (x axis) offset of this node respective to the parent node.
    ''' </summary>
    ''' <returns></returns>
    ''' <remarks>Used for visual layout of tree</remarks>
    Public Property OffsetToParent As Double = 0.0

    ''' <summary>
    ''' Relative position of the tree node on the X axis. Calculated from OffsetToParent in a separate process.
    ''' </summary>
    ''' <returns></returns>
    ''' <remarks>This value is not automatically updated if OffsetToParent changes</remarks>
    Public Property RelativeX As Double = 0D

    ''' <summary>
    ''' Relative position of the tree node on the Y axis. Taken from Level.
    ''' </summary>
    ''' <returns></returns>
    Public Property RelativeY As Double = 0D

    ''' <summary>
    ''' Absolute position of the node relative to the forest.
    ''' </summary>
    ''' <returns></returns>
    Public Property AbsoluteX As Double = 0D

    ''' <summary>
    ''' Absolute position of the node relative to the forest.
    ''' </summary>
    ''' <returns></returns>
    Public Property AbsoluteY As Double = 0D

    ''' <summary>
    ''' This node and all decendant nodes have had their horizontal offsets calculated
    ''' </summary>
    ''' <returns></returns>
    Public Property OffsetsInitialised As Boolean = False

    ''' <summary>
    ''' Set the absolute position of this treenode and all child nodes
    ''' </summary>
    ''' <param name="ParentRelativeX"></param>
    Public Sub SetRelativePositions(ByVal ParentRelativeX As Double)
        RelativeX = ParentRelativeX + OffsetToParent
        RelativeY = Level
        For Each Child As TreeNode In Children()
            Child.SetRelativePositions(RelativeX)
        Next
    End Sub

    ''' <summary>
    ''' Get the horizontal offset on a given contour (Side) at a given level
    ''' </summary>
    ''' <param name="Depth">How far down the tree respective to the current node should we go down?</param>
    ''' <param name="Side">left or right side of the contour</param>
    ''' <param name="MinOffset">The minimum amount of offset permitted between two nodes on the x axis.</param>
    ''' <returns>The cumulative offset at that depth (not just that depth's offset to parent)</returns>
    Public Function GetCummulativeOffset(ByVal Depth As Integer,
                              ByVal Side As ContourSide,
                              ByVal MinOffset As Integer) As Double

        ' Only answer once initialised. This is important as the subtree needs to be inititialised to know the answer.
        ' This is true except for where Depth = 0 but we still initialise to keep the behaviour consistent.
        If Not OffsetsInitialised Then
            InitialiseOffsets(MinOffset)
        End If

        If Depth = 0 Then
            Return OffsetToParent
        Else
            ' Work out which subtree to follow
            Dim MyChildren As List(Of TreeNode) = Children()      ' Cache the result 
            MyChildren.Sort()
            If Side = ContourSide.Left Then
                For i As Integer = 0 To MyChildren.Count - 1
                    If MyChildren(i).GenerationCount >= Depth - 1 Then
                        ' This tree is deep enough. Itteratively get the total offset.
                        Return MyChildren(i).GetCummulativeOffset(Depth - 1, ContourSide.Left, MinOffset) + OffsetToParent
                    End If
                Next
            Else
                ' Contour side Right
                For i As Integer = MyChildren.Count - 1 To 0 Step -1
                    If MyChildren(i).GenerationCount >= Depth - 1 Then
                        ' This tree is deep enough. Itteratively get the total offset.
                        Return MyChildren(i).GetCummulativeOffset(Depth - 1, ContourSide.Right, MinOffset) + OffsetToParent
                    End If
                Next
            End If
            ' If we get to this point then we have not found a subtree deep enough to get the required offset.
            Throw New ApplicationException("Tree node offset could not be calcuated.")
        End If

    End Function


    ''' <summary>
    ''' Calculate the offsets of all children and descendants relative to this node. 
    ''' </summary>
    ''' <param name="MinOffset">The minimum amount of offset permitted between two nodes on the x axis.</param>
    ''' <remarks>Relative offsets only.</remarks>
    Public Sub InitialiseOffsets(ByVal MinOffset As Integer)
        OffsetToParent = 0.0D
        Dim MyChildren As List(Of TreeNode) = Children()  ' Cache the result as this list is required a lot in this method and we do not want to run the query each time
        If MyChildren.Count = 0 Then
            ' Nothing to do.
            OffsetsInitialised = True
        ElseIf MyChildren.Count = 1 Then
            ' there is a single child below the current node so no need to calculate offsets. Just ensure child is initialised.
            If Not MyChildren(0).OffsetsInitialised Then
                MyChildren(0).InitialiseOffsets(MinOffset)
            End If
            OffsetsInitialised = True
        Else
            Dim ChildOffsets As New List(Of Double)      ' List of offsets from one child to another. Each ChildOffset entry contains the relative offset between the corresponding child and the next child. Therefore ChildOffsets should have size Children.Count-1
            Dim CurrOffset As Double                        ' Current offset between nodes while itterating
            Dim MaxOffset As Double                       ' The maxmimum required offset at root computed as we itterate down two subtrees
            Dim MaxDepth As Integer         ' Maximum depth that we need to traverse to in each subtree
            Dim MaxClumpDepth As Integer    ' Maximum depth of the 'clump'. see 'Generalisation to m-ary Trees and Forests in Tidier Drawings of Trees by Edward M. Reingold and John S. Tilford'
            Dim ClumpChild As TreeNode      ' The child from the clump of children to the left of the right hand side treenode. We are searching for the right contour of the clump.
            Dim TotalOffsets As Double      ' Total amount of offset
            Dim IntervalOffset As Double    ' The sum of offsets between the ClumpChild and the Child being evaluated.
            Dim ChildOffsetIntervalStart As Integer
            Dim ChildOffsetIntervalEnd As Integer

            MyChildren.Sort()     ' Ensure that the children are sorted according to their SortPosition. This means that calls to this method should be preceeded by a suitable sort process.
            For I As Integer = 0 To MyChildren.Count - 2
                MaxOffset = 0   ' Initialise
                ' As we move from the first child through to the last child, all children that have been processed become the left tree 'clump' 
                ' (see 'Generalisation to m-ary Trees and Forests in Tidier Drawings of Trees by Edward M. Reingold and John S. Tilford')
                ' This clump then has to be compared to the next child (right tree) so that we get the lower depths of any child in the clump. 
                ' For example, if we have three children, first has depth 3, second has depth 2, and third has depth 3, then
                ' the first and second children get compared as far as depth 2 (max depth of second child) whereas when the third child
                ' is assessed, it is assessed against the 'clump' of the first child and second child and therefore has to be 
                ' assessed to depth 3 as both the first and third child have this depth. This is how 'threads' (see threaded binary trees) work with m-ary trees. 
                MaxClumpDepth = 0
                For j As Integer = 0 To I
                    If MyChildren(j).GenerationCount > MaxClumpDepth Then
                        ' get the maximum clump depth
                        MaxClumpDepth = MyChildren(j).GenerationCount
                    End If
                Next

                MaxDepth = Math.Min(MaxClumpDepth, MyChildren(I + 1).GenerationCount)       ' you only need to traverse the depth of the shortest tree to calculate the offset of two subtrees
                ' Initialise the children I am about to traverse
                If Not MyChildren(I).OffsetsInitialised Then
                    ' Only hit on first pass when I = 0
                    MyChildren(I).InitialiseOffsets(MinOffset)
                End If
                If Not MyChildren(I + 1).OffsetsInitialised Then
                    MyChildren(I + 1).InitialiseOffsets(MinOffset)
                End If
                For Depth As Integer = 0 To MaxDepth
                    ' starting at the root of the two sub trees, drill down itteratively to the required depth.
                    ' find the correct child from the 'clump' for the current depth.
                    ' This is done by finding the furthest right branch of the left subtree that is deep enough (long enough) for the required depth.
                    ClumpChild = Nothing        ' initialise
                    For j As Integer = I To 0 Step -1
                        If MyChildren(j).GenerationCount >= Depth Then
                            ClumpChild = MyChildren(j)
                            Exit For
                        End If
                    Next
                    If IsNothing(ClumpChild) Then
                        Throw New ApplicationException("Clump child cannot be found")
                    End If

                    ' Interval offsets
                    ' we are about to calculate the offset between MyChildren(I + 1) and the ClumpChild, ClumpChild having an index less that I + 1.
                    ' The ClumpChild may already have offsets for it recorded in ChildOffsets but these offsets will not have been applied to 
                    ' the TreeNode yet so the ClumpChild will currently has OffsetToParent of 0 regardless of what we calculated And stored in ChildOffsets. 
                    ' Therefore, the offset for MyChildren(I + 1) is what we calculate it to be in CurrOffset, minus the sum of all offsets 
                    ' between the ClumpChild and MyChildren(I + 1) (which as we said have not been applied to ClumpChild yet).
                    ' We shall sum these 'unapplied' offsets here for subtraction later so that we do not duplicate offsets in ChildOffset.
                    IntervalOffset = 0D     ' Initialise
                    ChildOffsetIntervalStart = MyChildren.IndexOf(ClumpChild)
                    ChildOffsetIntervalEnd = I - 1
                    If Not ChildOffsetIntervalEnd < ChildOffsetIntervalStart Then
                        For x As Integer = ChildOffsetIntervalStart To ChildOffsetIntervalEnd
                            IntervalOffset += ChildOffsets(x)       ' sum the child offsets between the clumpchild and treenode to be evaluated.
                        Next
                    End If

                    ' Itterate through the right contour of the left tree (MyChildren(i)) and the left contour of the right tree (MyChildren(i+1)).
                    ' Calculation below reads as CurrOffset = RightTree.LeftContour - LeftTree.RightContour 
                    CurrOffset = MyChildren(I + 1).GetCummulativeOffset(Depth, ContourSide.Left, MinOffset) - ClumpChild.GetCummulativeOffset(Depth, ContourSide.Right, MinOffset)
                    If MaxOffset < MinOffset - CurrOffset - IntervalOffset Then
                        ' Store largest required offset.
                        MaxOffset = MinOffset - CurrOffset - IntervalOffset
                    End If
                Next
                ' Store the offset required between these two children
                ChildOffsets.Add(MaxOffset)
            Next
            ' Set all of the child offsets relative to me. 
            ' We know that the first child will be offset (negatively) half of the total required offsets. For example, if the total required
            ' offsets was 1, then the first child would be offset -(1/2) = -0.5.
            TotalOffsets = ChildOffsets.Sum()
            For i As Integer = 0 To MyChildren.Count - 1
                If i = 0 Then
                    ' first child
                    MyChildren(i).OffsetToParent = (TotalOffsets / 2D) * -1
                Else
                    ' All children after the first child
                    MyChildren(i).OffsetToParent = MyChildren(i - 1).OffsetToParent + ChildOffsets(i - 1)
                End If
            Next
            OffsetsInitialised = True
        End If
    End Sub

    ''' <summary>
    ''' Returns all direct parents of this node. Should be one node for a tree unless a loop is detected.
    ''' </summary>
    ''' <returns>All treenodes above this node</returns>
    Public Function Parents() As List(Of TreeNode)
        Return LinkedNodes.Where(Function(ln) ln.Level = Me.Level - 1).ToList()
    End Function
    ''' <summary>
    ''' Get all direct children of this node
    ''' </summary>
    ''' <returns>All treenodes below this node</returns>
    Public Function Children() As List(Of TreeNode)
        Return LinkedNodes.Where(Function(ln) ln.Level = Me.Level + 1).ToList()
    End Function

    ''' <summary>
    ''' Get all children of this node at all levels
    ''' </summary>
    ''' <returns>All treenodes below this node</returns>
    Public Function Descendants() As List(Of TreeNode)

        Dim Output As New List(Of TreeNode)     ' The return variable
        GetDescendantsItteratively(Output)
        Return Output
    End Function

    ''' <summary>
    ''' Get descendants itteratively.
    ''' </summary>
    ''' <param name="Output">An instanciated list of treeview. Method fails if parameter is nothing </param>
    ''' <remarks>Do not call directly. Use function Descendants() in all cases</remarks>
    Public Sub GetDescendantsItteratively(ByRef Output As List(Of TreeNode))
        If IsNothing(Output) Then
            Throw New ApplicationException("Output cannot be nothing")
        End If

        Dim MyChildren As List(Of TreeNode) = Children()
        If Not IsNothing(MyChildren) AndAlso MyChildren.Count > 0 Then
            ' Add the grandchild treenodes  
            For Each Child As TreeNode In MyChildren
                Child.GetDescendantsItteratively(Output)
            Next
        End If

        ' add the child treenodes
        For Each Child As TreeNode In MyChildren
            ' Ensures that the object Output is in order of depth (children, then grandchildren, etc.)
            Output.Insert(0, Child)
        Next

    End Sub


    ''' <summary>
    ''' Get all parents of this node at all levels
    ''' </summary>
    ''' <returns>All treenodes below this node</returns>
    Public Function Ancestors() As List(Of TreeNode)
        Throw New NotImplementedException("I thought this would be simply but now realise that it is in inverse of Descendants() and I need to implement this later")
        'Return LinkedNodes.Where(Function(ln) ln.Level < Me.Level).ToList()
    End Function

    ''' <summary>
    ''' direct descendants of the parent. Current node excluded.
    ''' </summary>
    ''' <returns></returns>
    Public Function Siblings() As List(Of TreeNode)
        Dim MyParents As List(Of TreeNode) = Parents()          ' Parents of this node
        Dim MyParentsChildren As List(Of TreeNode) = Nothing    ' Children of my parents (my siblings including me)
        If IsNothing(MyParents) OrElse MyParents.Count = 0 Then
            Return Nothing
        Else
            MyParentsChildren = MyParents.SelectMany(Function(p) p.Children).ToList()
            If Not IsNothing(MyParentsChildren) AndAlso MyParentsChildren.Count > 1 Then
                ' MyParentsChildren.Count > 1 because a count of 1 would just be me.
                Return MyParentsChildren.Where(Function(mpc) mpc.ElementId <> ElementId).ToList()
            Else
                Return Nothing
            End If
        End If
    End Function

    ''' <summary>
    ''' How many generations of children exist below this node  
    ''' </summary>
    ''' <returns></returns>
    Public Function GenerationCount() As Integer
        Dim MyDescendants As List(Of TreeNode) = Descendants()
        If IsNothing(Descendants) OrElse Descendants.Count = 0 Then
            Return 0
        Else
            Return Descendants.Max(Function(d) d.Level) - Me.Level
        End If
    End Function

    ''' <summary>
    ''' Create a join or link between two treenodes. 
    ''' </summary>
    ''' <param name="Node"></param>
    ''' <remarks>Attempts to create existing joins are ignored.</remarks>
    Public Sub Link(ByVal Node As TreeNode)
        If LinkedNodes.Where(Function(x) x.ElementId = Node.ElementId).Count = 0 Then
            ' Node has not already been added to our collection
            LinkedNodes.Add(Node)
        End If

        If Node.LinkedNodes.Where(Function(x) x.ElementId = ElementId).Count = 0 Then
            ' we have not been added to the linked nodes collection
            Node.LinkedNodes.Add(Me)
        End If
    End Sub

    ''' <summary>
    ''' Remove a join between two treenodes. 
    ''' </summary>
    ''' <param name="Node"></param>
    ''' <remarks>Attempts to remove non-existant joins are ignored.</remarks>
    Public Sub Unlink(ByVal Node As TreeNode)
        ' Remove all local references to the remote node
        For i As Integer = LinkedNodes.Count - 1 To 0 Step -1
            If LinkedNodes(i).ElementId = Node.ElementId Then
                LinkedNodes.RemoveAt(i)
            End If
        Next

        ' Rmeove all references to me in the remote node.
        For i As Integer = Node.LinkedNodes.Count - 1 To 0 Step -1
            If Node.LinkedNodes(i).ElementId = Me.ElementId Then
                Node.LinkedNodes.RemoveAt(i)
            End If
        Next
    End Sub

    Public Sub New()

    End Sub

    ''' <summary>
    ''' Constructor with source element ID
    ''' </summary>
    ''' <param name="ElementId">References the element ID in the source map</param>
    Public Sub New(ByVal ElementId As Integer)
        Me.ElementId = ElementId
    End Sub

    ''' <summary>
    ''' Default comparitor to be used in sorts and similar commands.
    ''' </summary>
    ''' <param name="other"></param>
    ''' <returns></returns>
    Public Function CompareTo(other As TreeNode) As Integer Implements IComparable(Of TreeNode).CompareTo
        If Me.SortPosition = other.SortPosition Then
            Return Me.ElementId.CompareTo(other.ElementId)
        Else
            Return Me.SortPosition.CompareTo(other.SortPosition)
        End If
    End Function

    ''' <summary>
    ''' build the post order traversal list of the subtree with current node as root.
    ''' </summary>
    ''' <param name="Output"></param>
    Public Sub GetPostOrderList(ByRef Output As List(Of TreeNode))

        Dim Children As List(Of TreeNode)       ' Children of the current node

        If IsNothing(Output) Then
            Throw New ApplicationException("Call to GetPostOrderList without output object")
        End If

        ' Add all of my children to the list
        Children = Me.Children()
        If Not IsNothing(Children) AndAlso Children.Count > 0 Then
            For Each Child As TreeNode In Children
                GetPostOrderList_Recursion(Child, Output)
            Next
        End If

        ' Add me to the list
        Output.Add(Me)

    End Sub

    ''' <summary>
    ''' Recursive component of the post order traversal list generator
    ''' </summary>
    ''' <param name="Node"></param>
    ''' <param name="Output"></param>
    Private Sub GetPostOrderList_Recursion(ByRef Node As TreeNode,
                                 ByRef Output As List(Of TreeNode))
        GetPostOrderList(Output)
    End Sub
End Class



#Region "CustomComparitors"
''' <summary>
''' Compare two treenodes based on count of immediate children
''' </summary>
Public Class CompareChildrenCount : Inherits Comparer(Of TreeNode)
    Public Overrides Function Compare(x As TreeNode, y As TreeNode) As Integer
        If IsNothing(x.Children) And IsNothing(y.Children) Then
            ' Both have no children, so they are equal.
            Return 0
        ElseIf IsNothing(x.Children) Then
            ' x has no children, so y has more. x < y
            Return -1
        ElseIf IsNothing(y.Children) Then
            ' x has children but y has none. x > y
            Return 1
        Else
            Return x.Children.Count.CompareTo(y.Children.Count)
        End If
    End Function
End Class
''' <summary>
''' Compare two treenodes based on count of immediate children
''' </summary>
Public Class CompareDescendantCount : Inherits Comparer(Of TreeNode)
    Public Overrides Function Compare(x As TreeNode, y As TreeNode) As Integer
        If IsNothing(x.Descendants) And IsNothing(y.Descendants) Then
            ' Both have no children, so they are equal.
            Return 0
        ElseIf IsNothing(x.Descendants) Then
            ' x has no children, so y has more. x < y
            Return -1
        ElseIf IsNothing(y.Descendants) Then
            ' x has children but y has none. x > y
            Return 1
        Else
            Return x.Descendants.Count.CompareTo(y.Descendants.Count)
        End If
    End Function
End Class
''' <summary>
''' Compare two treenodes based on how many generations of children they have (depth of tree nodes below current node)
''' </summary>
Public Class CompareGenerationCount : Inherits Comparer(Of TreeNode)
    Public Overrides Function Compare(x As TreeNode, y As TreeNode) As Integer
        Return x.GenerationCount.CompareTo(y.GenerationCount)
    End Function
End Class
#End Region
