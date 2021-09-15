Public Class IpUtil

    ''' <summary>
    ''' Attempt to parse a range of IP addresses
    ''' </summary>
    ''' <param name="IpDeclarationToParse">The IP range(s) declared in a string</param>
    ''' <param name="IpRanges">List of IP address upper / lower pairs in integer format</param>
    ''' <returns></returns>
    Public Shared Function TryParseIpRanges(ByVal IpDeclarationToParse As String,
                                    ByRef IpRanges As List(Of IpRange)) As Boolean

        Dim ParsedIpRange As IpRange
        Dim NetMask As UInt32
        Dim TempInt As Integer

        If IsNothing(IpRanges) Then
            Debug.Print("IpRanges is nothing")
            Return False
        End If

        If IpDeclarationToParse = "" Then
            Return False
        End If

        IpDeclarationToParse = IpDeclarationToParse.Replace(" ", "")

        Dim IpRangesToParse() As String = IpDeclarationToParse.Split(",")     ' In case of multiple ranges, each range is split by a comma
        Dim IpRangeTerms As List(Of String)

        For i As Integer = 0 To IpRangesToParse.Count - 1
            ' itterate each IP address range.
            If InStr(IpRangesToParse(i), "-") > 0 Then
                ' Hyphen found in IP range declaration, so range of IP addresses in form of "192.168.4.0 - .255" detected.
                IpRangeTerms = IpRangesToParse(i).Split("-").ToList
                If IpRangeTerms.Count <> 2 Then
                    Debug.Print("IpRangeTerms <> 2")
                    Return False
                End If
                ParsedIpRange = New IpRange()
                ' parse the first part of the range (192.168.4.0)
                If Not TryParseIpAddressToInteger(IpRangeTerms(0), ParsedIpRange.IpLower) Then
                    ' Parsing failed
                    Debug.Print("Failed to parse IP address '{0}' to integer", IpRangeTerms(0))
                    Return False
                Else
                    ' Parsing success
                    ' Calculate the end of range from the second term
                    Dim LowerOctetStrings As List(Of String) = IpRangeTerms(0).Split(".").ToList
                    Dim UpperOctetStrings As List(Of String) = IpRangeTerms(1).Split(".").ToList
                    For x As Integer = UpperOctetStrings.Count - 1 To 0 Step -1
                        ' remove blank entries in the upper octet list as ".255" will create two entries {"", "255"}
                        If UpperOctetStrings(x) = "" Then
                            UpperOctetStrings.RemoveAt(x)
                        Else
                            If Integer.TryParse(UpperOctetStrings(x), TempInt) Then
                                If TempInt < 0 Or TempInt > 255 Then
                                    ' Octet out of range
                                    Debug.Print("Octet '{0}' out of range", TempInt)
                                    Return False
                                End If
                            Else
                                ' Octet is not a valid integer  
                                Debug.Print("Octet '{0}' is not a valid integer", UpperOctetStrings(x))
                                Return False
                            End If
                        End If
                    Next
                    If UpperOctetStrings.Count = 0 Then
                        Debug.Print("UpperOctetStrings.Count = 0")
                        Return False
                    End If
                    ' Add octets from the lower octet string collection into the upper octet collection starting at the left hand side.
                    ' for example, add "192", then "168", then "4" then exit
                    For Each Octet As String In LowerOctetStrings
                        If UpperOctetStrings.Count < 4 Then
                            UpperOctetStrings.Insert(LowerOctetStrings.IndexOf(Octet), Octet)
                        Else
                            Exit For
                        End If
                    Next
                    If Not TryParseIpAddressToInteger(String.Join(".", UpperOctetStrings), ParsedIpRange.IpUpper) Then
                        Debug.Print("Failed to parse IP address '{0}' to integer", String.Join(".", UpperOctetStrings))
                        Return False
                    Else
                        If ParsedIpRange.IpUpper < ParsedIpRange.IpLower Then
                            Debug.Print("End of range is less that start")
                            Return False
                        Else
                            IpRanges.Add(ParsedIpRange)
                        End If
                    End If
                End If
            ElseIf InStr(IpRangesToParse(i), "/") > 0 Then
                ' CIDR notation such as "192.168.4.0/24" detected.
                IpRangeTerms = IpRangesToParse(i).Split("/").ToList
                If IpRangeTerms.Count <> 2 Then
                    Debug.Print("IpRangeTerms.Count <> 2")
                    Return False
                End If

                If IsNumeric(IpRangeTerms(1)) AndAlso (CInt(IpRangeTerms(1)) > -1 And CInt(IpRangeTerms(1)) < 33) Then
                    ' CIDR Mask is valid. Create bit mask for subnet
                    NetMask = 0
                    ' we do 32 - the CIDR mask so that we can populate the inverse number of bits from the right hand side.
                    ' for example, if the CIDR mask is /24, then we populate 8 bits (32-24) from the right hand side.
                    Dim x As Integer
                    Do While x < 32 - CInt(IpRangeTerms(1))
                        NetMask = NetMask << 1
                        NetMask = NetMask Or Convert.ToUInt32(1)
                        x += 1
                    Loop
                Else
                    Debug.Print("'{0}' is not a valid CIDR term.", IpRangeTerms(1))
                    Return False
                End If

                ParsedIpRange = New IpRange()
                ' parse the first part of the range (192.168.4.0)
                If Not TryParseIpAddressToInteger(IpRangeTerms(0), ParsedIpRange.IpLower) Then
                    ' Parsing failed
                    Debug.Print("Failed to parse IP address '{0}' to integer", IpRangeTerms(0))
                    Return False
                End If

                ' Apply the inverse of the net mask to the first IP range. This ensures that the start of the range is valid according to the CIDR mask.
                ' for example, 192.168.4.0/16 has range start 192.168.0.0, not 192.168.4.0
                ParsedIpRange.IpLower = ParsedIpRange.IpLower And (NetMask Xor &HFFFFFFFF)

                ' calculate the end of the valid IP range via bitwise masking.
                ParsedIpRange.IpUpper = ParsedIpRange.IpLower Or NetMask
                If ParsedIpRange.IpUpper < ParsedIpRange.IpLower Then
                    Debug.Print("End of range is less that start")
                    Return False
                Else
                    IpRanges.Add(ParsedIpRange)
                End If
            Else
                ' Single IP address
                ParsedIpRange = New IpRange()
                If TryParseIpAddressToInteger(IpRangesToParse(i), ParsedIpRange.IpLower) Then
                    ' only a single address and was parsed correctly, so copy lower to upper as they are both the same for a single address
                    ParsedIpRange.IpUpper = ParsedIpRange.IpLower
                    IpRanges.Add(ParsedIpRange)
                Else
                    Debug.Print("Failed to parse IP address '{0}' to integer", IpRangesToParse(i))
                    Return False
                End If
            End If
        Next
        If IpRanges.Count > 0 Then
            Return True
        Else
            Debug.Print("IpRnages.Count = 0")
            Return False
        End If
    End Function

    ''' <summary>
    ''' Convert a text IP Address (e.g. 192.168.4.0) to its integer equivelent
    ''' </summary>
    ''' <returns></returns>
    Public Shared Function TryParseIpAddressToInteger(ByVal IpString As String, ByRef IpInt As UInt32) As Boolean
        Dim OctetStrings As List(Of String) = IpString.Split(".").ToList        ' split string into four strings each containing the numbers. e.g. {"192", "168", "4", "0"}
        Dim OctetInt As UInt32
        Dim OctetShift As Integer() = {24, 16, 8, 0} ' the number of places to bit shift each octet

        If OctetStrings.Count <> 4 Then
            Debug.Print("IP address does not contain four octets")
            Return False
        End If

        ' Parse each string octet into its numeric equivelent
        For i As Integer = 0 To 3
            If Integer.TryParse(OctetStrings(i), OctetInt) Then
                If OctetInt > -1 And OctetInt < 256 Then
                    ' Bit shift the integer left the correct number of places for the position of the octet in the string and then add
                    ' that to the UInt32 return value.
                    ' e.g. 192 is converted to UInt32 and then bit shifted left 24 places. Then 168 is bit shifted left 16 places and they are added together
                    IpInt += OctetInt << OctetShift(i)
                Else
                    Debug.Print("Octet '{0}' is out of range", OctetInt)
                    Return False
                End If
            Else
                Debug.Print("Failed to parse to int on '{0}'", OctetStrings(i))
                Return False
            End If
        Next

        Return True

    End Function

    ''' <summary>
    ''' Test if the IP address is in the given range
    ''' </summary>
    ''' <param name="IpAddress"></param>
    ''' <param name="IpRange"></param>
    ''' <returns></returns>
    Public Shared Function IpAddressInRange(ByVal IpAddress As String, ByVal IpRange As IpRange) As Boolean
        Dim IpInt As UInt32
        If TryParseIpAddressToInteger(IpAddress, IpInt) Then
            Return IpAddressInRange(IpInt, IpRange)
        Else
            Debug.Print("Failed to parse IP address '{0}' to integer", IpAddress)
            Return False
        End If
    End Function

    ''' <summary>
    ''' Test if the IP address is in the given range
    ''' </summary>
    ''' <param name="IpAddress"></param>
    ''' <param name="IpRange"></param>
    ''' <returns></returns>
    Public Shared Function IpAddressInRange(ByVal IpAddress As UInt32, ByVal IpRange As IpRange) As Boolean
        If Not (IpAddress < IpRange.IpLower) And Not (IpAddress > IpRange.IpUpper) Then
            Return True
        Else
            Return False
        End If
    End Function

    ''' <summary>
    ''' Check if the given IP address is within any of the supplied ranges
    ''' </summary>
    ''' <param name="IpAddress"></param>
    ''' <param name="IpRanges"></param>
    ''' <returns></returns>
    Public Shared Function IpAddressInRanges(ByVal IpAddress As UInt32, ByVal IpRanges As List(Of IpRange)) As Boolean
        For Each Range As IpRange In IpRanges
            If IpAddressInRange(IpAddress, Range) Then
                Return True
            End If
        Next
        Return False
    End Function

    ''' <summary>
    ''' Check if the given IP address is within any of the supplied ranges
    ''' </summary>
    ''' <param name="IpAddress"></param>
    ''' <param name="IpRanges"></param>
    ''' <returns></returns>
    Public Shared Function IpAddressInRanges(ByVal IpAddress As String, ByVal IpRanges As List(Of IpRange)) As Boolean
        Dim IpInt As UInt32
        If TryParseIpAddressToInteger(IpAddress, IpInt) Then
            Return IpAddressInRanges(IpInt, IpRanges)
        Else
            Debug.Print("Failed to parse IP address '{0}' to integer", IpAddress)
            Return False
        End If
    End Function

End Class
