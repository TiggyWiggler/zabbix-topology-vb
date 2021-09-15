Imports ZabbixTopology.Common
Imports ZabbixTopology.Common.Enums
''' <summary>
''' Manages the layout and positioning of nodes within a populated map.
''' </summary>
Public Class MapLayout
    ''' <summary>
    ''' Layout a pre-populated map.
    ''' </summary>
    ''' <param name="SourceMap">prepopulated map. Format is specific to the requirements of Zabbix. To create a generalised form of this method this variable should be generalised (generalisation is done within this class anyway). Map is a forest of unconnected trees or one tree.</param>
    ''' <param name="NodeXSpace">internode spacing within trees (X axis)</param>
    ''' <param name="NodeYSpace">internode spacing within trees (Y axis)</param>
    ''' <param name="SortingStrategy">Collection of SortingMethods in order of preferance.</param>
    Public Shared Sub LayoutMap(ByRef SourceMap As Map,
                         ByVal NodeXSpace As Double,
                         ByVal NodeYSpace As Double,
                         ByVal TreePadding() As Integer,
                         ByVal SortingStrategy As List(Of SortingMethod))

        ' Currently implements the Tidier Drawings of Trees. Reingold & Tilford 1981. IEEE Transactions on Software Engineering Vol SE-7, No. 2 March 1981.
        ' However I would like it to later implement both Ringed Interactive Navigation Graph System (RINGS) Soon Tee Teoh & Kwan-Liu Ma 2002. Universite of California.
        ' and Radial Free Tree from Graph Drawing Tutorial. Isabel F. Cruz & Roberto Tamassia page 31.

        Dim Forest As Forest
        Dim MaxX As Double = 0D
        Dim MaxY As Double = 0D

        ' Validate parameters
        If IsNothing(SourceMap) Then
            Throw New ApplicationException("Map not defined")
        ElseIf IsNothing(SourceMap.selements) OrElse SourceMap.selements.Count = 0 Then
            Throw New ApplicationException("Map element collection empty or not defined")
        ElseIf NodeXSpace < 0.0 Then
            Throw New ApplicationException("X axis internode spacing cannot be negative")
        ElseIf NodeYSpace < 0.0 Then
            Throw New ApplicationException("Y axis internode spacing cannot be negative")
        End If

        ' Transfer all source map elements into the generalised form of a forest with trees of treenodes. 
        ' Treenodes will be grouped in trees as a consequence.
        Forest = MapToForest(SourceMap)

        If IsNothing(Forest) Then
            Debug.Print("Forest could not be built from source map. Exiting method now, I am expecting this to not have any effect on the output. Check other faults if this behaviour is unexpected.")
            Return
        End If

        LayoutForest(Forest, NodeXSpace, NodeYSpace, TreePadding, SortingStrategy)

        UpdateMapFromForest(Forest, SourceMap)

        ' calculate size of map.
        For Each T As Tree In Forest.Trees
            If T.Width + T.AbsoluteX > MaxX Then
                MaxX = T.Width + T.AbsoluteX
            End If
            If T.Height + T.AbsoluteY > MaxY Then
                MaxY = T.Height + T.AbsoluteY
            End If
        Next

        SourceMap.width = MaxX
        SourceMap.height = MaxY

    End Sub

    ''' <summary>
    ''' Layout a pre-populated forest.
    ''' </summary>
    ''' <param name="Forest">prepopulated forest. Forest is a collection of one or more unconnected trees</param>
    ''' <param name="NodeXSpace">internode spacing within trees (X axis)</param>
    ''' <param name="NodeYSpace">internode spacing within trees (Y axis)</param>
    ''' <param name="SortingStrategy">Collection of SortingMethods in order of preferance.</param>
    Public Shared Sub LayoutForest(ByRef Forest As Forest,
                         ByVal NodeXSpace As Double,
                         ByVal NodeYSpace As Double,
                         ByVal TreePadding() As Integer,
                         ByVal SortingStrategy As List(Of SortingMethod))

        Dim RootNode As TreeNode

        ' Validate parameters
        If IsNothing(Forest) Then
            Throw New ApplicationException("Forest not defined")
        ElseIf IsNothing(Forest.Trees) OrElse Forest.Trees.Count = 0 Then
            Throw New ApplicationException("Forest tree collection empty or not defined")
        ElseIf NodeXSpace < 0.0 Then
            Throw New ApplicationException("X axis internode spacing cannot be negative")
        ElseIf NodeYSpace < 0.0 Then
            Throw New ApplicationException("Y axis internode spacing cannot be negative")
        End If

        ' for each tree identify a root
        If Forest.Trees.Count > 0 Then
            SetRoots(Forest)
        Else
            Return
        End If

        ' for each tree calculate the depth of each node from the root.
        SetLevels(Forest)

        ' Sort the treenodes acording to the sorting strategy
        If Not IsNothing(SortingStrategy) Then
            For Each T As Tree In Forest.Trees
                For Each SM As SortingMethod In SortingStrategy
                    SortTree(T, SM)
                Next
            Next
        End If

        ' Draw tree
        For Each t As Tree In Forest.Trees
            RootNode = t.GetRootNode
            If Not IsNothing(RootNode) Then
                RootNode.InitialiseOffsets(1D)
            End If
            t.SetNodesRelative()    ' Convert offset to parent of each tree node to positioning relative to the container tree (node can be 0,0 within tree)

            ' Change the forest positioning from using single unit space to using desired spacing.
            For Each N As TreeNode In t.Nodes
                N.RelativeX *= NodeXSpace
                N.RelativeY *= NodeYSpace
            Next

            t.CalculateExtents()    ' Calculate the size of the tree
            ApplyTreePadding(t, TreePadding)        ' Applies padding to the tree
        Next

        Forest.ArrangeTrees()       ' position the trees within the forest.

        For Each t As Tree In Forest.Trees
            ' Position nodes with absolute reference to the forest.
            t.SetNodesAbsolute()
        Next

    End Sub

    ''' <summary>
    ''' Apply the padding to the tree
    ''' </summary>
    ''' <param name="T"></param>
    ''' <param name="TreePadding"></param>
    ''' <remarks>Applies padding in final dimensions (not unit space)</remarks>
    Private Shared Sub ApplyTreePadding(ByRef T As Tree, ByVal TreePadding() As Integer)
        Dim LeftPad As Integer = 0      ' Padding on left of tree
        Dim TopPad As Integer = 0       ' Padding on top of tree
        Dim BottomPad As Integer = 0    ' Padding on bottom of tree
        Dim RightPad As Integer = 0     ' Padding on right of tree
        If IsNothing(TreePadding) Then
            Exit Sub
        Else
            ' Extract desired padding
            Select Case UBound(TreePadding)
                Case 0
                    ' 1 term
                    LeftPad = TreePadding(0)
                    TopPad = TreePadding(0)
                    BottomPad = TreePadding(0)
                    RightPad = TreePadding(0)
                Case 1
                    ' 2 terms
                    TopPad = TreePadding(0)
                    BottomPad = TreePadding(0)
                    LeftPad = TreePadding(1)
                    RightPad = TreePadding(1)
                Case 2
                    ' 3 terms
                    TopPad = TreePadding(0)
                    RightPad = TreePadding(1)
                    BottomPad = TreePadding(2)
                Case 3
                    ' 4 terms
                    TopPad = TreePadding(0)
                    RightPad = TreePadding(1)
                    BottomPad = TreePadding(2)
                    LeftPad = TreePadding(3)
                Case Else
                    ' Illegal
                    Return      ' Fail silently
            End Select
            ' Map starts at 0,0 at the top left.
            If TopPad > 0 Then
                For Each N As TreeNode In T.Nodes
                    N.RelativeY += TopPad
                Next
            End If
            If LeftPad > 0 Then
                For Each N As TreeNode In T.Nodes
                    N.RelativeX += LeftPad
                Next
            End If
            ' Apply padding to the right and bottom of the tree
            T.Width += LeftPad + RightPad
            T.Height += TopPad + BottomPad
        End If
    End Sub


    ''' <summary>
    ''' Order the tree nodes
    ''' </summary>
    ''' <param name="T">The tree to be sorted</param>
    ''' <param name="SortMethod">The method to be used in sorting.</param>
    ''' <remarks>If the treenodes have not been sorted before (SortPosition=-1) then all nodes are sorted according to the method. 
    ''' If nodes have been sorted already (SortPosition!=-1) then only sibling nodes of the same SortPosition value are sorted respective to each other.
    ''' NOTE: Should this be part of the tree object?</remarks>
    Public Shared Sub SortTree(ByRef T As Tree,
                         ByVal SortMethod As SortingMethod)
        ' When each node is sorted it will be given a value of SortPosition within the scope of its siblings. This means that a parent and child can both have sort position '3'
        ' but this means nothing about their position to each other. However two siblings with sort position '3' are in the same order. A parent with sort position '2' and a child
        ' with sort position '3' means nothing to each other. However if one sibling is sort position '2' and another sibling is sort position '3' then the sibling with sort 
        ' position '2' is sorted higher (further to the left on a simple tree).
        ' IMPORTANT: When multiple siblings are at the same sort position, the next position after that skips the number of siblings at the previous level that were at
        ' the same sort position. This allows successive passes of sorting to sort within the group of siblings that were at the same level without having to reposition
        ' the siblings that were higher or lower. for example, assume these are all siblings:
        ' ElementId     |   SortPosition
        ' 1             |   0
        ' 2             |   1
        ' 3             |   2
        ' 4             |   2
        ' 5             |   2
        ' 6             |   5
        ' 7             |   6
        ' Here 3, 4, and 5 were all given position 2. This means that in the next sorting pass 3, 4, and 5 can be sorted against each other without affecting 1, 2, 6, or 7.

        Dim CurrentDepth As Integer = 0             ' Level from root (0=root)
        Dim CurrentSortPosition As Integer = 0      ' Memory of sort position.
        Dim NextSortPosition As Integer = 1         ' Memory of next sort position.
        Dim NodesAtDepth As List(Of TreeNode) = Nothing     ' Nodes that are at the current depth. 
        Dim NodesToSort As List(Of TreeNode) = Nothing      ' Nodes with same current sort position within the current depth.
        Dim MinSortPosition As Integer                      ' Minimum used sort position within nodes at this level
        Dim MaxSortPosition As Integer                      ' Maximum used sort position within nodes at this level.
        Dim PositionSearch As Integer               ' What sort position should we retrieve into NodesToSort
        Dim Comparitor As Comparer(Of TreeNode)     ' Custom comparitor object. -1 if sort is ascending, 0 if elements are equal, 1 if sort is descending.

        If Not IsNothing(T) AndAlso Not IsNothing(T.Nodes) AndAlso T.Nodes.Count > 0 Then
            ' Instanciate the comparitor
            Select Case SortMethod
                Case SortingMethod.TreeNodeChildren, SortingMethod.TreeNodeChildren_Desc
                    Comparitor = New CompareChildrenCount
                Case SortingMethod.TreeNodeDescendants, SortingMethod.TreeNodeDescendants_Desc
                    Comparitor = New CompareDescendantCount
                Case SortingMethod.TreeNodeMaxDepth, SortingMethod.TreeNodeMaxDepth_Desc
                    Comparitor = New CompareGenerationCount
                Case Else
                    Comparitor = New CompareChildrenCount       ' Default. Silences IDE warnings too.
            End Select

            Do While T.Nodes.Where(Function(n) n.Level = CurrentDepth).Count > 0
                NodesAtDepth = T.Nodes.Where(Function(n) n.Level = CurrentDepth).ToList()
                If NodesAtDepth.Count > 0 Then
                    ' We have nodes
                    ' Get the minimum and maximum of sort positions currently being used
                    MinSortPosition = NodesAtDepth.Min(Function(nad) nad.SortPosition)
                    MaxSortPosition = NodesAtDepth.Max(Function(nad) nad.SortPosition)
                    For PositionSearch = MinSortPosition To MaxSortPosition
                        ' Get all nodes at a given sort position so that we can sort within them
                        NodesToSort = NodesAtDepth.Where(Function(nad) nad.SortPosition = PositionSearch).ToList()
                        If NodesToSort.Count = 1 Then
                            If NodesToSort(0).SortPosition = -1 Then
                                ' Only one element and it is undefined at present. Set it to 0 (first position)
                                ' otherwise it has already been sorted and can be ignored.
                                NodesToSort(0).SortPosition = 0
                            End If
                        Else
                            ' We have multiple nodes to sort between.
                            NodesToSort.Sort(Comparitor)
                            Select Case SortMethod
                                Case SortingMethod.TreeNodeChildren_Desc, SortingMethod.TreeNodeDescendants_Desc, SortingMethod.TreeNodeMaxDepth_Desc
                                    NodesToSort.Reverse()
                            End Select

                            ' If we have just searched for undefined positions then we should set the first sorted element to 0. Otherwise the first sorted position should just remain 
                            ' where it was (position search)
                            If PositionSearch = -1 Then
                                CurrentSortPosition = 0
                                NextSortPosition = 1
                            Else
                                CurrentSortPosition = PositionSearch
                                NextSortPosition = PositionSearch + 1
                            End If

                            ' Taking our example from the head of this module we find ourselves with the elements that were all in sort position 2 (elements 3, 4, 5).
                            ' ElementId     |   SortPosition
                            ' 1             |   0
                            ' 2             |   1
                            ' 3             |   2
                            ' 4             |   2
                            ' 5             |   2
                            ' 6             |   5
                            ' 7             |   6
                            ' These elements are now sorted using whatever method was desired. 
                            ' CurrentSortPosition = 2, And NextSortPosition = 3.
                            ' We will now evaluate element 3 against element 4. if element 4 is greater (sorts higher) than element 3
                            ' the element 4 will be given the value of NextSortPosition, otherwise element 4 will be given CurrentSortPosition
                            ' and NextSortPosition will be incremented to maintain the spacing and ordering. 
                            ' It is not possible for element 4 to sort lower than 3 as the list has been sorted.

                            For i As Integer = 0 To NodesToSort.Count - 2       ' We compare current against next element, so count-2 prevents spilling over range.
                                If i = 0 And NodesToSort(i).SortPosition = -1 Then
                                    ' This For..Next is forward looking, it always deals with treenode i+1, so to avoid that we miss node i we 
                                    ' make a special check on the first loop and set the first node. Only  needed if NodesToSort(i).SortPosition = -1.
                                    ' If NodesToSort(i).SortPosition != -1 then NodesToSort(i).SortPosition already equals CurrentSortPosition
                                    NodesToSort(i).SortPosition = CurrentSortPosition
                                End If
                                If Comparitor.Compare(NodesToSort(i), NodesToSort(i + 1)) = 0 Then
                                    ' nodes are equal.
                                    NodesToSort(i + 1).SortPosition = CurrentSortPosition
                                Else
                                    ' nodes are ascending.
                                    NodesToSort(i + 1).SortPosition = NextSortPosition
                                    CurrentSortPosition = NextSortPosition
                                End If
                                NextSortPosition += 1
                            Next
                        End If
                    Next
                End If

                CurrentDepth += 1   ' Itterate
            Loop
        End If
    End Sub

    ''' <summary>
    ''' Set the distance from the root for all nodes in all trees in the forest
    ''' </summary>
    ''' <param name="F">Forest containing trees</param>
    ''' <remarks>root must exist for each applicable tree. Root has level 0, all other nodes must be level -1 or else they will not be assessed or traversed</remarks>
    Public Shared Sub SetLevels(ByRef F As Forest)
        If Not IsNothing(F) AndAlso Not IsNothing(F.Trees) AndAlso F.Trees.Count > 0 Then
            For Each T As Tree In F.Trees
                If T.Nodes.Count > 0 Then
                    If T.Nodes.Where(Function(n) n.Level = 0).Count < 1 Then
                        Throw New ApplicationException("Call to SetLevels without root defined. At least one node must have level = 0")
                    End If
                    SetLevels(T)
                End If
            Next
        End If
    End Sub

    ''' <summary>
    ''' Set the distance from the root for each node in the tree    
    ''' </summary>
    ''' <param name="T">Tree that we will be setting levels for</param>
    Public Shared Sub SetLevels(ByRef T As Tree)
        Dim Rows As List(Of TreeNode)
        Dim I As Integer = 0

        If Not IsNothing(T) AndAlso T.Nodes.Count > 0 Then
            If T.Nodes.Where(Function(n) n.Level = 0).Count < 1 Then
                Throw New ApplicationException("Call to SetLevels without root defined. At least one node must have level = 0")
            End If
            Do
                ' Get all treenodes at the current level (I) and then pull all linked nodes that have not had their level set (level = -1)
                Rows = (From pn As TreeNode In T.Nodes Where pn.Level = I From ln In pn.LinkedNodes Where ln.Level = -1 Select ln).ToList

                If IsNothing(Rows) OrElse Rows.Count = 0 Then
                    Exit Do
                End If

                ' Set the level of each node to be one more than the current node
                For Each Child As TreeNode In Rows
                    Child.Level = I + 1
                Next

                I += 1      ' Increment for next loop
            Loop
        End If
    End Sub

    ''' <summary>
    ''' Set the root node for all trees in the forest
    ''' </summary>
    ''' <param name="F"></param>
    Public Shared Sub SetRoots(ByRef F As Forest)
        If Not IsNothing(F.Trees) AndAlso F.Trees.Count > 0 Then
            For Each T As Tree In F.Trees
                If Not IsNothing(T.Nodes) AndAlso T.Nodes.Count > 0 Then
                    SetRoot(T)
                End If
            Next
        End If
    End Sub

    ''' <summary>
    ''' Set the root node for the tree
    ''' </summary>
    ''' <param name="T"></param>
    Public Shared Sub SetRoot(ByRef T As Tree)
        Dim TempNode As TreeNode        ' Temporary memory for a single treenode
        If Not IsNothing(T.Nodes) AndAlso T.Nodes.Count > 0 Then
            ' get the tree node with the most connections. Select just the first one we get where multiple nodes share the highest count.
            TempNode = (From n In T.Nodes Order By n.LinkedNodes.Count() Descending).First()
            TempNode.Level = 0
        End If
    End Sub

    ''' <summary>
    ''' Convert a zabbix source map into a generalised forest
    ''' </summary>
    ''' <param name="SourceMap"></param>
    ''' <returns></returns>
    Private Shared Function MapToForest(ByRef SourceMap As Map) As Forest

        Dim Ret As Forest = Nothing
        Dim NewTree As Tree = Nothing
        Dim Node As TreeNode = Nothing
        Dim NodesWithoutTree As Tree = Nothing
        Dim NodeA As TreeNode = Nothing             ' one side of a node link
        Dim NodeB As TreeNode = Nothing             ' other side of a node link

        If IsNothing(SourceMap) Then
            ' Nothing was passed, so return nothing
            Return Ret
        End If

        Ret = New Forest

        If IsNothing(SourceMap.selements) OrElse SourceMap.selements.Count = 0 Then
            ' Blank or missing hosts, so return empty forest (no trees)
            Return Ret
        End If

        ' Copy all of the nodes from the source map into a single tree regardless of grouping etc. at the moment.
        NodesWithoutTree = New Tree

        For Each Element As Selement In SourceMap.selements
            Node = New TreeNode(Element.selementid)
            If IsNothing(NodesWithoutTree.Nodes) Then
                NodesWithoutTree.Nodes = New List(Of TreeNode)
            End If
            NodesWithoutTree.Nodes.Add(Node)
        Next

        ' Then the links between nodes
        If Not IsNothing(SourceMap.links) Then
            For Each Link As SelementLink In SourceMap.links
                ' The SelementLink will have references to two hosts which should now exist as nodes. Grab those two nodes
                NodeA = NodesWithoutTree.FindNodeByElementID(Link.selementid1)
                NodeB = NodesWithoutTree.FindNodeByElementID(Link.selementid2)
                If IsNothing(NodeA) Or IsNothing(NodeB) Then
                    ' The two nodes referenced by the SelemenetLink could not be found
                    Throw New ApplicationException("Source map links point to a node that was not found in the generalised list")
                End If
                ' Create the connection between the nodes. If the connection already exists this request will be ignored (fails silently).
                NodeA.Link(NodeB)
            Next
        End If

        ' group all linked nodes (nodes that are linked to other nodes) into a tree with all other nodes with which they are linked.
        ' Result is a tree with all nodes that are connected through n number of hops where n is any non-negative number.
        Do While NodesWithoutTree.Nodes.Where(Function(n) Not IsNothing(n.LinkedNodes) AndAlso n.LinkedNodes.Count > 0).Count > 0
            ' Get the subject node. This is any node that has linked nodes but has not been moved into a tree of its own yet.
            Node = NodesWithoutTree.Nodes.First(Function(n) Not IsNothing(n.LinkedNodes) AndAlso n.LinkedNodes.Count > 0)
            NewTree = New Tree
            NewTree.Nodes = New List(Of TreeNode)
            MoveNodeRecursively(NodesWithoutTree, NewTree, Node)
            If NewTree.Nodes.Count = 0 Then
                Throw New ApplicationException("Tree nodes unexpectantly empty")
            End If
            ' Copy newly populated tree into the return parameter.
            If IsNothing(Ret.Trees) Then
                Ret.Trees = New List(Of Tree)
            End If
            Ret.Trees.Add(NewTree)
        Loop

        ' Any nodes that are not within specific trees by this point are isolated nodes (not linked to any other node). 
        ' Each isolated node shall be placed in its own tree (tree with one node).
        ' This is so that the forest contains only trees.
        For Each Node In NodesWithoutTree.Nodes
            NewTree = New Tree
            NewTree.Nodes = New List(Of TreeNode)
            NewTree.Nodes.Add(Node)
            If IsNothing(Ret.Trees) Then
                Ret.Trees = New List(Of Tree)
            End If
            Ret.Trees.Add(NewTree)
        Next

        Return Ret
    End Function

    ''' <summary>
    ''' Take the generalised forest which was previously extracted from the Zabbix map and use the updated forest to update the Zabbix map.
    ''' </summary>
    ''' <param name="SourceForest">Forest which was previous created from the map</param>
    ''' <param name="TargetMap">The map which will be updated from the forest data</param>
    Private Shared Sub UpdateMapFromForest(ByRef SourceForest As Forest,
                                             ByRef TargetMap As Map)
        Dim MaxX As Double = 0D
        Dim MaxY As Double = 0D
        Dim N As TreeNode
        For Each SElement In TargetMap.selements
            ' Get the forest treenode that was generated from the current SElements
            N = SourceForest.FindNodeByElementID(SElement.selementid)
            If Not IsNothing(N) Then
                ' Copy over the position data from the forest treenode to the map selement
                With SElement
                    .x = N.AbsoluteX
                    .y = N.AbsoluteY
                End With
            End If
        Next
    End Sub

    ''' <summary>
    ''' Recursively move the given node plus all nodes linked to the given node from the source tree to the destination tree.
    ''' </summary>
    ''' <param name="SourceTree"></param>
    ''' <param name="TargetTree"></param>
    ''' <param name="Node"></param>
    ''' <remarks>Remove nodes from source tree as move is made</remarks>
    Private Shared Sub MoveNodeRecursively(ByRef SourceTree As Tree, ByRef TargetTree As Tree, ByVal Node As TreeNode)

        ' Try to find the node in the source tree. 
        Dim SourceNode As TreeNode = SourceTree.Nodes.FirstOrDefault(Function(n) n.ElementId = Node.ElementId)

        If Not IsNothing(SourceNode) Then
            ' We have found the node in the source tree.
            If Not IsNothing(TargetTree) AndAlso Not IsNothing(TargetTree.Nodes) AndAlso TargetTree.Nodes.Where(Function(n) n.ElementId = Node.ElementId).Count = 0 Then
                ' Not found in target tree already. Move it to the target tree. By using the source node rather than the passed node we avoid issues with broken references.
                TargetTree.Nodes.Add(SourceNode)
            End If
            ' Remove the node from the source table.
            SourceTree.Nodes.RemoveAt(SourceTree.Nodes.IndexOf(SourceNode))
            If Not IsNothing(SourceNode.LinkedNodes) AndAlso SourceNode.LinkedNodes.Count > 0 Then
                ' move the nodes linked to this node
                For Each LinkedNode As TreeNode In SourceNode.LinkedNodes
                    MoveNodeRecursively_Recursion(SourceTree, TargetTree, LinkedNode)
                Next
            End If
        Else
            ' We did not find the node in the source tree so we just return. This 'Else' command included for documentation only.
        End If

    End Sub

    ''' <summary>
    ''' Recursive component
    ''' </summary>
    ''' <param name="SourceTree"></param>
    ''' <param name="TargetTree"></param>
    ''' <param name="Node"></param>
    Private Shared Sub MoveNodeRecursively_Recursion(ByRef SourceTree As Tree, ByRef TargetTree As Tree, ByVal Node As TreeNode)
        MoveNodeRecursively(SourceTree, TargetTree, Node)
    End Sub


End Class
