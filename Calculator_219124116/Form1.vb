Imports Accessibility

Public Class Form1
    Public Structure Calculator
        Dim numberArr As ArrayList
        Dim signStack As Stack
    End Structure

    Dim bkCalulators As New ArrayList
    Dim InputStringArr As New ArrayList
    Dim InputString As String = ""

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim calculatorInstance As New Calculator With {
            .numberArr = New ArrayList,
            .signStack = New Stack
        }
        bkCalulators.Add(calculatorInstance)
        Label_window.Text = InputString
    End Sub

    Private Sub Button_n_0_Click(sender As Object, e As EventArgs) Handles Button_n_0.Click
        setLabelWindowIS("0")
    End Sub

    Private Sub setLabelWindowIS(v As String)
        '입력 조건

        '최대 길이 제한
        If InputString.Length < 29 * 2 Then
            If InputString.Length = 0 And Not IsNumeric(v) Then
                '첫입력에 기호가 오면
                If v = "(" Then
                    InputString = InputString & v
                Else
                    Return
                End If

            ElseIf InputString.Length > 0 And Not IsNumeric(v) Then
                If Not v = "(" And Not v = ")" And Not InputString.Last = "(" And Not InputString.Last = ")" And Not IsNumeric(InputString.Last) Then
                    Dim tmp As String = ""
                    Dim i As Integer

                    '기호뒤에 기호가오면 맨끝 기호 대신 새로운 기호를 넣음
                    For i = 0 To InputString.Length - 2 Step 1
                        tmp = tmp & (InputString(i))
                    Next i

                    '기호 추가 전 입력된 문자열에 새로 입력된 기호 추가
                    InputString = tmp & v
                Else
                    '그냥 기호 입력
                    InputString = InputString & v
                End If

            Else
                '문제 없으면 문자열 뒤에 추가
                InputString = InputString & v
            End If

            Label_window.Text = InputString
        Else
            MsgBox("최대 58개만 입력할 수 있습니다. " + vbCrLf + "더이상 입력할 수 없습니다.", 0, "경고")
        End If
    End Sub

    Private Sub Button_n_1_Click(sender As Object, e As EventArgs) Handles Button_n_1.Click
        setLabelWindowIS("1")
    End Sub

    Private Sub Button_n_2_Click(sender As Object, e As EventArgs) Handles Button_n_2.Click
        setLabelWindowIS("2")
    End Sub

    Private Sub Button_n_3_Click(sender As Object, e As EventArgs) Handles Button_n_3.Click
        setLabelWindowIS("3")
    End Sub

    Private Sub Button_n_4_Click(sender As Object, e As EventArgs) Handles Button_n_4.Click
        setLabelWindowIS("4")
    End Sub

    Private Sub Button_n_5_Click(sender As Object, e As EventArgs) Handles Button_n_5.Click
        setLabelWindowIS("5")
    End Sub

    Private Sub Button_n_6_Click(sender As Object, e As EventArgs) Handles Button_n_6.Click
        setLabelWindowIS("6")
    End Sub

    Private Sub Button_n_7_Click(sender As Object, e As EventArgs) Handles Button_n_7.Click
        setLabelWindowIS("7")
    End Sub

    Private Sub Button_n_8_Click(sender As Object, e As EventArgs) Handles Button_n_8.Click
        setLabelWindowIS("8")
    End Sub

    Private Sub Button_n_9_Click(sender As Object, e As EventArgs) Handles Button_n_9.Click
        setLabelWindowIS("9")
    End Sub

    Private Sub Button_t_3_Click(sender As Object, e As EventArgs) Handles Button_t_3.Click
        InputStringArr.Clear()
        InputString = ""
        Label_window.Text = InputString
        Label_1.Text = InputString
    End Sub

    Private Sub Button_t_4_Click(sender As Object, e As EventArgs) Handles Button_t_4.Click
        If InputString.Length > 0 Then
            InputString = InputString.Remove(InputString.Length - 1)
            Label_window.Text = InputString
        Else
            Label_window.Text = InputString
        End If
    End Sub

    Private Sub Button_t_1_Click(sender As Object, e As EventArgs) Handles Button_t_1.Click
        setLabelWindowIS("(")
    End Sub

    Private Sub Button_t_2_Click(sender As Object, e As EventArgs) Handles Button_t_2.Click
        setLabelWindowIS(")")
    End Sub

    Private Sub Button_t_5_Click(sender As Object, e As EventArgs) Handles Button_t_5.Click
        setLabelWindowIS("+")
    End Sub

    Private Sub Button_t_6_Click(sender As Object, e As EventArgs) Handles Button_t_6.Click
        setLabelWindowIS("-")
    End Sub

    Private Sub Button_t_7_Click(sender As Object, e As EventArgs) Handles Button_t_7.Click
        setLabelWindowIS("×")
    End Sub

    Private Sub Button_t_8_Click(sender As Object, e As EventArgs) Handles Button_t_8.Click
        setLabelWindowIS("÷")
    End Sub
    Private Sub setNumberArrFromStack(numberArr As ArrayList, signStack As Stack)
        Dim i As Integer
        For i = 0 To signStack.Count - 1 Step 1
            numberArr.Add(signStack.Pop)
        Next i
    End Sub


    Private Sub Button_t_result_Click(sender As Object, e As EventArgs) Handles Button_t_result.Click
        '일단 기본적인것만 우선 오류제어
        If InputString = "" Then
            Return
        ElseIf Not IsNumeric(InputString(InputString.Length - 1)) Then
            Select Case InputString(InputString.Length - 1)
                Case "+"
                    Return
                Case "-"
                    Return
                Case "×"
                    Return
                Case "÷"
                    Return
            End Select
        End If

        '입력된 식 분리하기
        setInputStringArr()

        '원본 계산식 저장 생성
        Dim openBracket As Integer = 0

        '후위 표기법 변환
        Dim i As Integer
        For i = 0 To InputStringArr.Count - 1 Step 1
            If IsNumeric(InputStringArr(i)) Then
                ' 숫자면 그냥 넣음
                bkCalulators(openBracket).numberArr.Add(InputStringArr(i))

            Else
                '기호면 몇개 체크해야함
                If InputStringArr(i) = "(" Then
                    '괄호가 열리면 
                    openBracket += 1
                    Dim calculatorInstance As New Calculator With {
                        .numberArr = New ArrayList,
                        .signStack = New Stack
                    }
                    bkCalulators.Add(calculatorInstance)

                ElseIf InputStringArr(i) = ")" Then
                    '괄호가 닫히면 삭제
                    setNumberArrFromStack(bkCalulators(openBracket).numberArr, bkCalulators(openBracket).signStack)
                    '이전 배열로 다 추가
                    Dim u As Integer
                    For u = 0 To bkCalulators(openBracket).numberArr.Count - 1 Step 1
                        bkCalulators(openBracket - 1).numberArr.Add(bkCalulators(openBracket).numberArr(u))
                    Next u
                    bkCalulators.RemoveAt(openBracket)
                    openBracket -= 1

                ElseIf bkCalulators(openBracket).signStack.Count = 0 And (InputStringArr(i) = "+" Or InputStringArr(i) = "-") Then
                    '스택의 처음 +-이런건 그냥 넣음
                    bkCalulators(openBracket).signStack.Push(InputStringArr(i))

                ElseIf InputStringArr(i) = "+" Or InputStringArr(i) = "-" Then
                    '그다음 +-는 스택이랑 교환
                    setNumberArrFromStack(bkCalulators(openBracket).numberArr, bkCalulators(openBracket).signStack)
                    bkCalulators(openBracket).signStack.Clear()
                    bkCalulators(openBracket).signStack.Push(InputStringArr(i))

                ElseIf InputStringArr(i) = "×" Or InputStringArr(i) = "÷" Then
                    '*/는 스택에 넣음
                    bkCalulators(openBracket).signStack.Push(InputStringArr(i))

                End If
            End If
        Next i

        '마지막 스택에 있는거 다넣음
        setNumberArrFromStack(bkCalulators(openBracket).numberArr, bkCalulators(openBracket).signStack)

        '후위연산 출력
        Dim tmp As String = ""
        For i = 0 To bkCalulators(openBracket).numberArr.Count - 1 Step 1
            tmp = tmp & bkCalulators(openBracket).numberArr(i)
        Next
        Label_1.Text = tmp

        '실제 계산
        '늦은바인딩에러떠서 배열을 새로만들어야함
        Dim result As New ArrayList
        result = setResultNumberArr(bkCalulators(openBracket).numberArr)
        Label_window.Text = CStr(result(0))

        bkCalulators(openBracket).numberArr.Clear()
        bkCalulators(openBracket).signStack.Clear()
        result.Clear()
        InputStringArr.Clear()
        InputString = ""

    End Sub

    Private Function setResultNumberArr(numberArr As ArrayList) As ArrayList
        '계산해서 배열 반환
        Dim tmp As Integer = 0
        Dim i As Integer = 0

        While Not numberArr.Count = 1
            If Not IsNumeric(numberArr(i)) Then

                Select Case numberArr(i)
                    Case "+"
                        tmp = CInt(numberArr(i - 2)) + CInt(numberArr(i - 1))
                    Case "-"
                        tmp = CInt(numberArr(i - 2)) - CInt(numberArr(i - 1))
                    Case "×"
                        tmp = CInt(numberArr(i - 2)) * CInt(numberArr(i - 1))
                    Case "÷"
                        tmp = CInt(numberArr(i - 2)) \ CInt(numberArr(i - 1))

                End Select

                numberArr(i - 2) = tmp
                numberArr.RemoveAt(i)
                numberArr.RemoveAt(i - 1)

                numberArr = setResultNumberArr(numberArr)
            End If

            i += 1
        End While
        Return numberArr
    End Function

    Private Sub setInputStringArr()
        '분리하기
        Dim tmp As String = ""
        Dim i As Integer
        For i = 0 To InputString.Length - 1 Step 1
            If Not IsNumeric(InputString(i)) Then
                InputStringArr.Add(tmp)
                tmp = ""
                InputStringArr.Add(InputString(i))
            Else
                tmp = tmp & InputString(i)
            End If
        Next i
        InputStringArr.Add(tmp)
    End Sub
End Class
