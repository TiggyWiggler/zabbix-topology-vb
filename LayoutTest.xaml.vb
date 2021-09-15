Imports ZabbixTopology.Common.Enums

Public Class LayoutTest
    Private Sub Button_Click(sender As Object, e As RoutedEventArgs)
        Dim AddPseudoHosts As Boolean = False
        Dim AddPseudoHubs As Boolean = False
        Dim BLL As New NetworkMapBLL
        BLL.DataSource = NetworkMapBLL.DataSourceType.FileStore
        BLL.FileCacheReadPath = "c:\temp"
        BLL.CreateHostMap("ShouldNotAppear",
                            2000,
                            2000,
                            "Hub_(64)",
                            "Switch_(64)",
                            "Workstation_(64)",
                            AddPseudoHosts,
                            AddPseudoHubs,,)
    End Sub

    Private Function TestForest1() As Forest
        ' Test scenario 1
        Dim F As New Forest
        Dim T As New Tree
        F.Trees.Add(T)

        ' Simple Tree
        '       ----a----
        '       |       |
        '     --b--     c
        '     |   |      
        '     d   e

        Dim Node_a As New TreeNode
        Dim Node_b As New TreeNode
        Dim Node_c As New TreeNode
        Dim Node_d As New TreeNode
        Dim Node_e As New TreeNode
        Node_a.ElementId = 1
        Node_b.ElementId = 2
        Node_c.ElementId = 3
        Node_d.ElementId = 4
        Node_e.ElementId = 5

        Node_b.Link(Node_d)
        Node_b.Link(Node_e)
        Node_a.Link(Node_b)
        Node_a.Link(Node_c)
        Node_a.Level = 0


        T.Nodes.Add(Node_a)
        T.Nodes.Add(Node_b)
        T.Nodes.Add(Node_c)
        T.Nodes.Add(Node_d)
        T.Nodes.Add(Node_e)

        MapLayout.SetLevels(F)

        Dim SortMethod As SortingMethod = SortingMethod.TreeNodeDescendants_Desc

        MapLayout.SortTree(T, SortMethod)

        Node_a.InitialiseOffsets(1D)

        Return F
    End Function

    Private Function TestForest2() As Forest
        ' Test Offsets - 2
        Dim F As New Forest
        Dim T As New Tree
        F.Trees.Add(T)

        ' Medium Tree
        '   ---------0-----------
        '   |    |    |    |    |
        '   1  --2--  3    4    5
        '      |   |       |
        '      6   7     --8--
        '                |   |
        '                9   10
        Dim Node As TreeNode
        For i = 0 To 10
            Node = New TreeNode
            Node.ElementId = i
            T.Nodes.Add(Node)
        Next

        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(1)) ' a -> b
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(2)) ' a -> c
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(3)) ' a -> d
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(4)) ' a -> e
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(5)) ' a -> f
        T.FindNodeByElementID(2).Link(T.FindNodeByElementID(6)) ' c -> g
        T.FindNodeByElementID(2).Link(T.FindNodeByElementID(7)) ' c -> h
        T.FindNodeByElementID(4).Link(T.FindNodeByElementID(8)) ' e -> i
        T.FindNodeByElementID(8).Link(T.FindNodeByElementID(9)) ' i -> j
        T.FindNodeByElementID(8).Link(T.FindNodeByElementID(10)) ' i -> j

        T.FindNodeByElementID(0).Level = 0        ' Set root  

        MapLayout.SetLevels(F)

        Dim SortMethod As SortingMethod = SortingMethod.TreeNodeDescendants_Desc

        MapLayout.SortTree(T, SortMethod)

        T.FindNodeByElementID(0).InitialiseOffsets(1D)
        Return F
    End Function

    Private Function TestForest3() As Forest
        ' Test Offsets - 3
        Dim F As New Forest
        Dim T As New Tree
        F.Trees.Add(T)

        ' Large Tree
        '              ------------------0----------------
        '              |               |                  |
        '   -----------1-----------    2           -------3-------
        '   |          |          |    |           |             |
        '   4  --------5--------  6    7  ---------8-----------  9
        '      |   |   |   |   |          |   |   |   |   |   |                      
        '      10  11  12  13  14         15  16  17  18  19  20                
        Dim Node As TreeNode
        For i = 0 To 20
            Node = New TreeNode
            Node.ElementId = i
            Node.SortPosition = i
            T.Nodes.Add(Node)
        Next

        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(1)) ' 0 -> 1
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(2))
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(3))
        T.FindNodeByElementID(1).Link(T.FindNodeByElementID(4))
        T.FindNodeByElementID(1).Link(T.FindNodeByElementID(5))
        T.FindNodeByElementID(1).Link(T.FindNodeByElementID(6))
        T.FindNodeByElementID(2).Link(T.FindNodeByElementID(7))
        T.FindNodeByElementID(3).Link(T.FindNodeByElementID(8))
        T.FindNodeByElementID(3).Link(T.FindNodeByElementID(9))
        T.FindNodeByElementID(5).Link(T.FindNodeByElementID(10))
        T.FindNodeByElementID(5).Link(T.FindNodeByElementID(11))
        T.FindNodeByElementID(5).Link(T.FindNodeByElementID(12))
        T.FindNodeByElementID(5).Link(T.FindNodeByElementID(13))
        T.FindNodeByElementID(5).Link(T.FindNodeByElementID(14))    ' 0 -> 14
        T.FindNodeByElementID(8).Link(T.FindNodeByElementID(15))
        T.FindNodeByElementID(8).Link(T.FindNodeByElementID(16))
        T.FindNodeByElementID(8).Link(T.FindNodeByElementID(17))
        T.FindNodeByElementID(8).Link(T.FindNodeByElementID(18))
        T.FindNodeByElementID(8).Link(T.FindNodeByElementID(19))
        T.FindNodeByElementID(8).Link(T.FindNodeByElementID(20))

        T.FindNodeByElementID(0).Level = 0        ' Set root  

        MapLayout.SetLevels(F)
        T.FindNodeByElementID(0).InitialiseOffsets(1D)
        Return F
    End Function

    Private Function TestForest4() As Forest
        ' Test Offsets - 4
        Dim F As New Forest
        Dim T As New Tree
        F.Trees.Add(T)

        ' Agorithm TR Example tree
        '                   --------0--------
        '                   |               |
        '               ----1----       ----2----
        '               |       |       |       |
        '           ----3----   4       5   ----6----
        '           |       |               |       |
        '       ----7----   8               9   ----10---
        '       |       |                       |       |
        '   ----11---   12                      13  ----14---
        '   |       |                               |       |
        '   15  ----16---                       ----17---   18
        '       |       |                       |       |
        '       19  ----20---               ----21---   22
        '           |       |               |       |
        '           23  ----24---       ----25---   26
        '               |       |       |       |
        '               27      28      29      30

        Dim Node As TreeNode
        For i = 0 To 30
            Node = New TreeNode
            Node.ElementId = i
            Node.SortPosition = i
            T.Nodes.Add(Node)
        Next

        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(1)) ' 0 -> 1
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(2))
        T.FindNodeByElementID(1).Link(T.FindNodeByElementID(3))
        T.FindNodeByElementID(1).Link(T.FindNodeByElementID(4))
        T.FindNodeByElementID(2).Link(T.FindNodeByElementID(5))
        T.FindNodeByElementID(2).Link(T.FindNodeByElementID(6))
        T.FindNodeByElementID(3).Link(T.FindNodeByElementID(7))
        T.FindNodeByElementID(3).Link(T.FindNodeByElementID(8))
        T.FindNodeByElementID(6).Link(T.FindNodeByElementID(9))
        T.FindNodeByElementID(6).Link(T.FindNodeByElementID(10))
        T.FindNodeByElementID(7).Link(T.FindNodeByElementID(11))
        T.FindNodeByElementID(7).Link(T.FindNodeByElementID(12))
        T.FindNodeByElementID(10).Link(T.FindNodeByElementID(13))
        T.FindNodeByElementID(10).Link(T.FindNodeByElementID(14))
        T.FindNodeByElementID(11).Link(T.FindNodeByElementID(15))
        T.FindNodeByElementID(11).Link(T.FindNodeByElementID(16))
        T.FindNodeByElementID(14).Link(T.FindNodeByElementID(17))
        T.FindNodeByElementID(14).Link(T.FindNodeByElementID(18))
        T.FindNodeByElementID(16).Link(T.FindNodeByElementID(19))
        T.FindNodeByElementID(16).Link(T.FindNodeByElementID(20))
        T.FindNodeByElementID(17).Link(T.FindNodeByElementID(21))
        T.FindNodeByElementID(17).Link(T.FindNodeByElementID(22))
        T.FindNodeByElementID(20).Link(T.FindNodeByElementID(23))
        T.FindNodeByElementID(20).Link(T.FindNodeByElementID(24))
        T.FindNodeByElementID(21).Link(T.FindNodeByElementID(25))
        T.FindNodeByElementID(21).Link(T.FindNodeByElementID(26))
        T.FindNodeByElementID(24).Link(T.FindNodeByElementID(27))
        T.FindNodeByElementID(24).Link(T.FindNodeByElementID(28))
        T.FindNodeByElementID(25).Link(T.FindNodeByElementID(29))
        T.FindNodeByElementID(25).Link(T.FindNodeByElementID(30))

        T.FindNodeByElementID(0).Level = 0        ' Set root  

        MapLayout.SetLevels(F)
        T.FindNodeByElementID(0).InitialiseOffsets(1D)
        Return F
    End Function

    Private Function TestForest5() As Forest
        ' Test Offsets - 5
        Dim F As New Forest
        Dim T As New Tree
        F.Trees.Add(T)

        ' 10.17.104.0/24 & Map non-hosts. 
        ' 
        ' 
        '           ------------0-----------
        '           |                      |
        '       ----1----              ----2----
        '       |       |              |       |
        '       3   ----4----          5       6
        '           |       |
        '           7       8
        '                   |
        '                   9


        Dim Node As TreeNode
        For i = 0 To 9
            Node = New TreeNode
            Node.ElementId = i
            Node.SortPosition = i
            T.Nodes.Add(Node)
        Next

        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(1)) ' 0 -> 1
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(2))
        T.FindNodeByElementID(1).Link(T.FindNodeByElementID(3))
        T.FindNodeByElementID(1).Link(T.FindNodeByElementID(4))
        T.FindNodeByElementID(2).Link(T.FindNodeByElementID(5))
        T.FindNodeByElementID(2).Link(T.FindNodeByElementID(6))
        T.FindNodeByElementID(4).Link(T.FindNodeByElementID(7))
        T.FindNodeByElementID(4).Link(T.FindNodeByElementID(8))
        T.FindNodeByElementID(8).Link(T.FindNodeByElementID(9))


        T.FindNodeByElementID(0).Level = 0        ' Set root  

        MapLayout.SetLevels(F)
        T.FindNodeByElementID(0).InitialiseOffsets(1D)
        Return F
    End Function

    Private Function TestForest6() As Forest
        ' Test Offsets - 6
        Dim F As New Forest
        Dim T As New Tree
        F.Trees.Add(T)

        ' 10.17.104.50-.90 & Map non-hosts. 
        ' 
        ' 
        '           ----------------------0------------------
        '           |               |           |           |
        '       ----1----       ----2----   ----3----   ----4----   
        '       |       |       |       |   |       |   |       |
        '   ----6----   5       7       8   9       10  11      12
        '   |       |       
        '   14      13      
        '   |                
        '   15               


        Dim Node As TreeNode
        For i = 0 To 15
            Node = New TreeNode
            Node.ElementId = i
            T.Nodes.Add(Node)
        Next

        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(1)) ' 0 -> 1
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(2))
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(3))
        T.FindNodeByElementID(0).Link(T.FindNodeByElementID(4))
        T.FindNodeByElementID(1).Link(T.FindNodeByElementID(5))
        T.FindNodeByElementID(1).Link(T.FindNodeByElementID(6))
        T.FindNodeByElementID(2).Link(T.FindNodeByElementID(7))
        T.FindNodeByElementID(2).Link(T.FindNodeByElementID(8))
        T.FindNodeByElementID(3).Link(T.FindNodeByElementID(9))
        T.FindNodeByElementID(3).Link(T.FindNodeByElementID(10))
        T.FindNodeByElementID(4).Link(T.FindNodeByElementID(11))
        T.FindNodeByElementID(4).Link(T.FindNodeByElementID(12))
        T.FindNodeByElementID(6).Link(T.FindNodeByElementID(13))
        T.FindNodeByElementID(6).Link(T.FindNodeByElementID(14))
        T.FindNodeByElementID(14).Link(T.FindNodeByElementID(15))

        T.FindNodeByElementID(0).Level = 0        ' Set root  

        MapLayout.SetLevels(F)

        Dim SortMethod As SortingMethod = SortingMethod.TreeNodeChildren

        MapLayout.SortTree(T, SortMethod)

        T.FindNodeByElementID(0).InitialiseOffsets(1D)
        Return F
    End Function

    Private Sub Button_Click_1(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest1()
        Dim T As Tree = F.Trees(0)
        PrintTreeNodeData(T)
    End Sub

    Private Sub Button_Click_2(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest2()
        Dim T As Tree = F.Trees(0)
        PrintTreeNodeData(T)
    End Sub

    Private Sub Button_Click_3(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest3()
        Dim T As Tree = F.Trees(0)
        PrintTreeNodeData(T)
    End Sub

    Private Sub Button_Click_4(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest4()
        Dim T As Tree = F.Trees(0)
        PrintTreeNodeData(T)
    End Sub

    Private Sub Button_Click_5(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest1()
        Dim T As Tree = F.Trees(0)
        RenderTree(T)
    End Sub

    Private Sub Button_Click_6(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest2()
        Dim T As Tree = F.Trees(0)
        RenderTree(T)
    End Sub

    Private Sub Button_Click_7(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest3()
        Dim T As Tree = F.Trees(0)
        RenderTree(T)
    End Sub

    Private Sub Button_Click_8(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest4()
        Dim T As Tree = F.Trees(0)
        RenderTree(T)
    End Sub

    Private Sub OffsetTest5(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest5()
        Dim T As Tree = F.Trees(0)
        PrintTreeNodeData(T)
    End Sub

    Private Sub AbsoluteTest5(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest5()
        Dim T As Tree = F.Trees(0)
        RenderTree(T)
    End Sub

    Private Sub OffsetTest6(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest6()
        Dim T As Tree = F.Trees(0)
        PrintTreeNodeData(T)
    End Sub

    Private Sub AbsoluteTest6(sender As Object, e As RoutedEventArgs)
        Dim F As Forest = TestForest6()
        Dim T As Tree = F.Trees(0)
        RenderTree(T)
    End Sub

    ''' <summary>
    ''' Calculate the dimensions of the tree and position the nodes within the tree, then output the results to the Output window.
    ''' </summary>
    ''' <param name="T"></param>
    Private Sub RenderTree(ByRef T As Tree)
        T.SetNodesRelative()
        T.CalculateExtents()
        T.SetNodesAbsolute()
        PrintTreeNodeData(T)
        PrintTreeData(T)
    End Sub

    Public Shared Sub PrintTreeNodeData(ByRef T As Tree)
        Debug.Print("----- Tree Node Data -----")
        For Each N As TreeNode In T.Nodes
            Debug.Print("ElementId {0} has X position {1} and Y position {2} from Level {3} and OffsetToParent {4}.", N.ElementId, N.AbsoluteX, N.AbsoluteY, N.Level, N.OffsetToParent)
        Next
    End Sub

    Public Shared Sub PrintTreeData(ByRef T As Tree)
        Debug.Print("----- Tree Data -----")
        Debug.Print("Tree as width {0} and height {1}", T.Width, T.Height)
    End Sub
End Class
