Public Class Form1
    'Gweerthoedd eang sy'n cael eu defnyddio mewn mwy nag un modiwl
    Const NiferLlysoedd = 4 'nifer o lysoedd
    Const NiferSafleoedd = 4 'nifer posib o safleoedd ym mhod cystadleuaeth
    Dim SafleoeddLlys(NiferLlysoedd, NiferSafleoedd) As Integer 'Cyfrif y nifer o safleoedd cafodd pob llys
    Dim CyfanswmSgorLlys(NiferLlysoedd) As Integer 'Cyfanswm sgor pob llys
    Dim EnwauLlysoedd(NiferLlysoedd) As String 'Storio enwau llysoed ynnhrefn yr wyddor

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim llys As Integer 'rheoli dolen ar gyfer pob llys
        Dim safle As Integer 'rheoli dolen safle
        'Rhoi enwau i bob llys
        EnwauLlysoedd(0) = "Dewi"
        EnwauLlysoedd(1) = "Dyfrig"
        EnwauLlysoedd(2) = "Illtud"
        EnwauLlysoedd(3) = "Teilo"

        'Gwerthcychwynol o 0 ar gyfer y cyfanswm sgor a safleoedd y llysoedd
        For llys = 0 To NiferLlysoedd - 1
            'Gosod cyfanswm sgor y llys i 0
            CyfanswmSgorLlys(llys) = 0
            'Gosod y cyfrif o safleoedd y llys i 0
            For safle = 0 To NiferSafleoedd - 1
                SafleoeddLlys(llys, safle) = 0
            Next
        Next
        'Enwau Llysoedd

        arddangosDrosDro(CyfanswmSgorLlys, SafleoeddLlys)
    End Sub

    Private Sub arddangosDrosDro(ByVal Cyfanswm As Object, ByVal Safleoedd As Object)
        Dim llys As Integer 'rheoli dolen ar gyfer pob llys
        Dim safle As Integer 'rheoli dolen safle

        lblCanlyniadauDrosDro.Text = "Sgor y Llysoedd" & vbCrLf
        For llys = 0 To NiferLlysoedd - 1
            'Allbynu cyfanswm sgor y llys 
            lblCanlyniadauDrosDro.Text = lblCanlyniadauDrosDro.Text & EnwauLlysoedd(llys) & ": " & Cyfanswm(llys) & vbCrLf
        Next

        'Llbynnu y cyfrif o safleoedd y llys
        lblCanlyniadauDrosDro.Text = lblCanlyniadauDrosDro.Text & "Safleoedd" & vbCrLf
        For llys = 0 To NiferLlysoedd - 1
            lblCanlyniadauDrosDro.Text = lblCanlyniadauDrosDro.Text & EnwauLlysoedd(llys) & ": "
            For safle = 0 To NiferSafleoedd - 1
                lblCanlyniadauDrosDro.Text = lblCanlyniadauDrosDro.Text & Safleoedd(llys, safle) & "  "
            Next
            lblCanlyniadauDrosDro.Text = lblCanlyniadauDrosDro.Text & vbCrLf
        Next
    End Sub

    Private Sub btnYchwanegu_Click(sender As Object, e As EventArgs) Handles btnYchwanegu.Click
        'Mewnbynnu ac ychwanegu data
        Dim canlyniadau(NiferSafleoedd) As Integer
        Dim llys As Integer 'dolen llys
        'Dim Dewi As Integer 'Safle Dewi
        'Dim Dyfrig As Integer 'Safle Dyfrig
        'Dim Illtud As Integer 'Safle Illtud
        'Dim Teilo As Integer 'Safle Teilo
        'darllen y safleoedd


        canlyniadau(0) = txtDewi.Text
        canlyniadau(1) = txtDyfrig.Text
        canlyniadau(2) = txtIlltud.Text
        canlyniadau(3) = txtTeilo.Text

        'Diweddaru'r sgor a safleoedd
        For llys = 0 To NiferLlysoedd - 1
            'Ychwanegu at y cyfanswm
            CyfanswmSgorLlys(llys) = CyfanswmSgorLlys(llys) + canlyniadau(llys)
            'Ychwanegu at y safle y llys a'r sgor yny blwch testun yw'r safle i adio 1 ato
            SafleoeddLlys(llys, canlyniadau(llys) - 1) = SafleoeddLlys(llys, canlyniadau(llys) - 1) + 1
        Next
        arddangosDrosDro(CyfanswmSgorLlys, SafleoeddLlys)
    End Sub

    Private Sub btnLlysBuddugol_Click(sender As Object, e As EventArgs) Handles btnLlysBuddugol.Click
        'Darganfod y llys buddugol
        Dim llysGydaSorIsaf(NiferLlysoedd) 'Rhif(au) y Llys(oedd) gyda'r sgor isaf
        Dim NiferGydaSgorIsaf As Integer = 0
      
        'Arddangos sgor pob llys
        arddangosSgor(CyfanswmSgorLlys)

        'Darganfod ennillydd
        'Darganfod y sgor isaf (mwyaf o safleoedd isel)
        Dim SgorIsaf As Integer = 9999
        Dim llys As Integer 'dolen llys
        For llys = 0 To NiferLlysoedd - 1
            'Cymharu cyfanswm sgor pob llys
            If CyfanswmSgorLlys(llys) < SgorIsaf Then ' Dyma'r sgor isaf mor belled
                'Newid y sgor iasf i sgor y llys yma
                SgorIsaf = CyfanswmSgorLlys(llys)
            End If
        Next
        'darganfod pa llys sydd a'r sgor isaf
        For llys = 0 To NiferLlysoedd - 1
            'Cymharu cyfanswm sgor pob llys
            If CyfanswmSgorLlys(llys) = SgorIsaf Then ' Llys yma'r sgor isaf
                'Ychwanegu rhif y llys i arae sy'n storio y llysoedd sydd a'r sgor isaf
                llysGydaSorIsaf(NiferGydaSgorIsaf) = llys
                'Ychwanegu un at y nifer gyda sgor isaf
                NiferGydaSgorIsaf = NiferGydaSgorIsaf + 1
            End If
        Next
        'Darganfod y nifer o ennillwyr
        If NiferGydaSgorIsaf = 1 Then ' Enillydd clir
            lblLlysBuddugol.Text = " Y llys buddugol yw " & EnwauLlysoedd(llysGydaSorIsaf(0))
        Else ' mwy nag un llys gyda'r un sgor
            'Gwirio os oes ennillydd oherwydd mwy o safleoedd cyntaf 
            'Dim safle As Integer = 0
            Call Cyfrifnol(llysGydaSorIsaf, NiferGydaSgorIsaf, SafleoeddLlys, 0)
            If NiferGydaSgorIsaf = 1 Then 'un llys sydd a'r nifer mwyaf oiafle 1
                lblLlysBuddugol.Text = " Y llys buddugol yw " & EnwauLlysoedd(llysGydaSorIsaf(0))
            Else ' mwy nag un llys gyda'r un sgor
                'Gwirio os oes ennillydd oherwydd mwy o ail safleoedd 
                Cyfrifnol(llysGydaSorIsaf, NiferGydaSgorIsaf, SafleoeddLlys, 1)
                If NiferGydaSgorIsaf = 1 Then 'un llys sydd a'r nifer mwyaf oiafle 1
                    lblLlysBuddugol.Text = " Y llys buddugol yw " & EnwauLlysoedd(llysGydaSorIsaf(0))
                Else ' mwy nag un llys gyda'r un sgor
                    'Gwirio os oes ennillydd oherwydd mwy o drydydd safleoedd 
                    Cyfrifnol(llysGydaSorIsaf, NiferGydaSgorIsaf, SafleoeddLlys, 3)
                    If NiferGydaSgorIsaf = 1 Then 'un llys sydd a'r nifer mwyaf oiafle 1
                        lblLlysBuddugol.Text = " Y llys buddugol yw " & EnwauLlysoedd(llysGydaSorIsaf(0))
                    Else ' mwy nag un llys gyda'r un sgor
                        lblLlysBuddugol.Text = "Dim ennillydd clir"
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub arddangosSgor(ByVal Cyfanswm As Object)
        'Arddangos cyfanswm sgor pob llys

        Dim llys As Integer 'rheoli dolen ar gyfer pob llys

        lblSgorLlysoedd.Text = "Sgor y Llysoedd" & vbCrLf
        For llys = 0 To NiferLlysoedd - 1
            'Allbynu cyfanswm sgor y llys 
            lblSgorLlysoedd.Text = lblSgorLlysoedd.Text & EnwauLlysoedd(llys) & ": " & Cyfanswm(llys) & vbCrLf
        Next
    End Sub

    Private Sub Cyfrifnol(ByRef llysoeddCyfartal As Object, ByRef Nifer As Integer, ByVal safleoeddllys As Object, ByVal safle As Object)
        'Nifer gyda'r un sgor
        'Darganfod y gwerth isaf yng ngholofn safle o'r arae safleoeddllys, ar gyfer y llysoedd sydd ynllysoeddcyfartal

        Dim llys As Integer 'rheoli'r ddolen
        Dim mwyaf As Integer = -1 'nifer isaf 
        Dim llysoeddmwyaf(NiferLlysoedd) As Integer ' llysoedd sydd wedi cael y nifer mwyaf o safleoedd
        Dim NiferNewydd As Integer = 0 'Nifer o llysoedd sydd wedi cael y nifer mwyaf o safle yma

        'Darganfod y nifer mwyaf o safleoedd
        For llys = 0 To Nifer - 1
            If safleoeddllys(llysoeddCyfartal(llys), safle) > mwyaf Then ' mwyaf newydd
                mwyaf = safleoeddllys(llysoeddCyfartal(llys), safle) 'gwerth mwyaf newydd
            End If
        Next
        'Darganfod  rhif(au) y llys(oedd) sydd wedi cael y nifer mwyaf o safleoedd
        For llys = 0 To Nifer - 1
            If safleoeddllys(llysoeddCyfartal(llys), safle) = mwyaf Then ' mwyaf newydd
                llysoeddmwyaf(llys) = llysoeddCyfartal(llys) 'gwerth mwyaf newydd
                NiferNewydd = NiferNewydd + 1 'Ychwanegu 1 at y nifer sydd wedi cael y nifer mwyaf o safle
            End If
        Next
        'Newid y gwerthoedd ar gyfer y rhestr o lysoedd sy'n gyfartal a'r nifer
        llysoeddCyfartal = llysoeddmwyaf
        Nifer = NiferNewydd
    End Sub
End Class
