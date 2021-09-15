
Namespace Common
    Public Class Enums
        Public Enum ChassisIdType
            ChassisComponent = 1
            InterfaceAlias = 2
            PortComponent = 3
            MacAddress = 4
            NetworkAddress = 5
            InterfaceName = 6
            Local = 7 ' Locally defined on switch. Can be anything.
        End Enum

        Public Enum PortIdType
            InterfaceAlias = 1
            PortComponent = 2
            MacAddress = 3
            NetworkAddress = 4
            InterfaceName = 5
            agentCircuitId = 6
            Local = 7 ' Locally defined on switch. Can be anything.
        End Enum

        Public Enum SortingMethod
            TreeNodeDescendants         ' Total number of children below a tree node regardless of depth (children, grandchildren, etc.). Ascending order
            TreeNodeDescendants_Desc    ' Total number of children below a tree node regardless of depth (children, grandchildren, etc.). Descending order
            TreeNodeChildren            ' Total number of children directly below current tree node (just children). Ascending order    
            TreeNodeChildren_Desc       ' Total number of children directly below current tree node (just children). Descending order
            TreeNodeMaxDepth            ' total depth of children below current tree node (number of generations). Ascending order
            TreeNodeMaxDepth_Desc       ' total depth of children below current tree node (number of generations). Descending order
        End Enum

    End Class
End Namespace

