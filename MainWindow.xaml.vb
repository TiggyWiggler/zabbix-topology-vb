Imports System.IO
Imports System.Net
Imports System.Runtime.InteropServices.WindowsRuntime
Imports System.Text
Imports Newtonsoft.Json
Imports Newtonsoft.Json.Linq
Imports ZabbixTopology.Common.Enums

Class MainWindow

    Private BLL As NetworkMapBLL = Nothing

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        BLL = New NetworkMapBLL

        If DataSourceWebApi.IsChecked Then
            BLL.DataSource = NetworkMapBLL.DataSourceType.WebApi
        Else
            BLL.DataSource = NetworkMapBLL.DataSourceType.FileStore
        End If

        If Not BLL.IsAuthenticated Then
            ' Add any initialization after the InitializeComponent() call.
            MessageTextbox.Text = "Enter credentials and press enter"
        Else
            If Not MapHostsButton.IsEnabled Then
                MapHostsButton.IsEnabled = True
            End If
        End If

    End Sub

    ''' <summary>
    ''' Connect to Zabbix server, authenticate and record token.
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    Protected Sub ConnectButton_Click(sender As Object, e As RoutedEventArgs)
        'Try
        ' Authenticate
        If UsernameTextbox.Text = "" Then
            Throw New ApplicationException("Username is missing")
        End If
        If PasswordTextbox.Text = "" Then
            Throw New ApplicationException("Password is missing")
        End If

        Try
            BLL.Authenticate(UsernameTextbox.Text, PasswordTextbox.Text)
            If BLL.IsAuthenticated Then
                ' Authorisation attempt successful
                MessageTextbox.Text = "Authorised"
                If Not MapHostsButton.IsEnabled Then
                    MapHostsButton.IsEnabled = True
                End If
            Else
                MessageTextbox.Text = "Authorisation failed without error"
                If MapHostsButton.IsEnabled Then
                    MapHostsButton.IsEnabled = False
                End If
            End If
        Catch ex As Exception
            MessageTextbox.Text = ex.Message
        End Try

    End Sub

    ''' <summary>
    ''' Create a map that contains all of our hosts.
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    Protected Sub MapHostsButton_Click(sender As Object, e As RoutedEventArgs)
        Dim MapName As String = MapNameTextbox.Text
        Dim AddPseudoHosts As Boolean           ' Should we add hosts that are detected via LLDP from a known host, but are not actually hosts themselves?
        Dim AddPseudoHubs As Boolean            ' Should we add hosts where we find multiple devices connected to a single port?
        Dim xSpacing As Double
        Dim ySpacing As Double
        Dim SortingMethod As SortingMethod = SortingMethod.TreeNodeChildren     ' Default state
        Dim TreePadding() As Integer = Nothing   ' Padding array starting from 12 O'clock position and working clockwise. One element is applied to all, two elements are applied vertically then horizontally, four elements are applied to the four faces
        Dim LinkLabels As Boolean               ' Should the links between map nodes include labels?

        ' Delete the map if already existing
        Dim Maps As List(Of Map) = BLL.GetMaps()
        If Not IsNothing(Maps) AndAlso Maps.Count > 0 Then
            For Each m As Map In Maps
                If m.name = MapName Then
                    BLL.DeleteMap(CInt(m.sysmapid))
                    Exit For
                End If
            Next
        End If

        If IsNumeric(NodeHSpaceTextbox.Text) Then
            If Not Double.TryParse(NodeHSpaceTextbox.Text, xSpacing) Then
                ' Default value
                xSpacing = 100D
            End If
        End If

        If IsNumeric(NodeVSpaceTextbox.Text) Then
            If Not Double.TryParse(NodeVSpaceTextbox.Text, ySpacing) Then
                ' Default value
                ySpacing = 100D
            End If
        End If

        ' Sorting strategy
        Select Case CType(SortMethodCombobox.SelectedItem, ComboBoxItem).Content
            Case "Children Count"
                SortingMethod = IIf(CType(SortDirectionCombobox.SelectedItem, ComboBoxItem).Content = "Ascending", SortingMethod.TreeNodeChildren, SortingMethod.TreeNodeChildren_Desc)
            Case "Descendant Count"
                SortingMethod = IIf(CType(SortDirectionCombobox.SelectedItem, ComboBoxItem).Content = "Ascending", SortingMethod.TreeNodeDescendants, SortingMethod.TreeNodeDescendants_Desc)
            Case "Generation Count"
                SortingMethod = IIf(CType(SortDirectionCombobox.SelectedItem, ComboBoxItem).Content = "Ascending", SortingMethod.TreeNodeMaxDepth, SortingMethod.TreeNodeMaxDepth_Desc)
        End Select

        AddPseudoHosts = MapNonHostsCheckbox.IsChecked
        AddPseudoHubs = PseudoHubsCheckbox.IsChecked
        TryParseTreePadding(TreePaddingTextbox.Text, TreePadding)
        LinkLabels = IncludeLinkLabelsCheckbox.IsChecked

        Try
            BLL.CreateHostMap(DisplayName:=MapName,
                              Width:=2000,
                              Height:=2000,
                              HubImageName:=HubImageTextbox.Text,
                              SwitchImageName:=SwitchImageTextbox.Text,
                              UnknownImageName:=UnknownImageTextbox.Text,
                              CreatePseudoHosts:=AddPseudoHosts,
                              CreatePseudoHubs:=AddPseudoHubs,
                              IpFilter:=IpFilterTextbox.Text,
                              NodeXSpace:=xSpacing,
                              NodeYSpace:=ySpacing,
                              SortingMethod:=SortingMethod,
                              TreePadding:=TreePadding,
                              IncludeLinkLabels:=LinkLabels)

            MessageTextbox.Text = "Map Created:" & vbCrLf & Now.ToString()
        Catch ex As Exception
            MessageTextbox.Text = ex.Message
        End Try
    End Sub

    ''' <summary>
    ''' Try to parse the tree padding field and put the result in the padding array
    ''' </summary>
    ''' <param name="PaddingText"></param>
    ''' <param name="PaddingArray"></param>
    ''' <returns>true if successful, false otherwise</returns>
    Private Function TryParseTreePadding(ByVal PaddingText As String,
                                         ByRef PaddingArray() As Integer) As Boolean
        ' Remove blank spaces
        PaddingText = PaddingText.Replace(" ", "")

        If PaddingText = "" Then
            ' create new empty array
            PaddingArray = New Integer() {}
            Return True
        Else
            ' Padding text contains some string
            Dim Terms As String() = PaddingText.Split(",")
            If UBound(Terms) > 3 Then
                ' Too many terms
                PaddingArray = Nothing
                Return False
            Else
                ' Size the target array according to the input string terms
                PaddingArray = New Integer(UBound(Terms)) {}
                For I As Integer = 0 To UBound(Terms)
                    If IsNumeric(Terms(I)) Then
                        If Not Integer.TryParse(Terms(I), PaddingArray(I)) Then
                            ' Could not parse as integer even though it was a number
                            PaddingArray = Nothing
                            Return False
                        End If
                    Else
                            PaddingArray = Nothing
                        Return False
                    End If
                Next
                Return True
            End If
        End If
    End Function

    Private Sub SelectFolder_Click(sender As Object, e As RoutedEventArgs)
        Using dialog As New System.Windows.Forms.FolderBrowserDialog
            Dim result As System.Windows.Forms.DialogResult
            result = dialog.ShowDialog
            If result = Forms.DialogResult.OK Then
                SelectedFolder.Text = dialog.SelectedPath
                BLL.FileCacheReadPath = dialog.SelectedPath
            End If
        End Using
    End Sub

    Private Sub DataSourceWebApi_Checked(sender As Object, e As RoutedEventArgs)
        If Not IsNothing(BLL) Then
            BLL.DataSource = NetworkMapBLL.DataSourceType.WebApi
        End If
    End Sub

    Private Sub DataSourceFileStore_Checked(sender As Object, e As RoutedEventArgs)
        If Not IsNothing(BLL) Then
            BLL.DataSource = NetworkMapBLL.DataSourceType.FileStore
        End If
    End Sub


End Class
