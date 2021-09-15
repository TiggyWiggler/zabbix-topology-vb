Public Interface ZabbixDal
    ''' <summary>
    ''' returns false if the user is not authenticated to access the data source (Zabbix web API for example)
    ''' </summary>
    ''' <returns></returns>
    Function IsAuthenticated() As Boolean

    ''' <summary>
    ''' Authenticate the user (login)
    ''' </summary>
    ''' <param name="UserName"></param>
    ''' <param name="Password"></param>
    Sub Authenticate(ByVal UserName As String,
                     ByVal Password As String)

    ''' <summary>
    ''' Get all maps from the data source
    ''' </summary>
    ''' <returns></returns>
    Function GetMaps() As List(Of Map)

    ''' <summary>
    ''' Generate map in the data source (e.g. push the map to Zabbix server)
    ''' </summary>
    ''' <param name="Map"></param>
    Sub CreateMap(Map As Map)

    ''' <summary>
    ''' Delete a map from the data source
    ''' </summary>
    ''' <param name="SysMapId"></param>
    Sub DeleteMap(SysMapId As Integer)

    ''' <summary>
    ''' Get image id for a given image name from retrieved image collection
    ''' </summary>
    ''' <param name="ImageName"></param>
    ''' <returns></returns>
    Function ImageId(ByVal ImageName As String) As Integer

    ''' <summary>
    ''' Get host devices from the data source
    ''' </summary>
    ''' <returns></returns>
    Function GetHosts() As List(Of HostDevice)

End Interface
