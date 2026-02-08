# Fichier: test_debug.ps1

$url = "http://localhost:8081/action"

# Vérifier si le serveur est actif
Write-Host "--- VÉRIFICATION DU SERVEUR ---" -ForegroundColor Cyan
try {
    $testConn = Invoke-WebRequest -Uri $url -Method Options -ErrorAction SilentlyContinue
    Write-Host "✓ Serveur actif sur port 8081" -ForegroundColor Green
} catch {
    Write-Host "✗ ERREUR: Serveur non accessible sur $url" -ForegroundColor Red
    Write-Host "Lancez le serveur: swipl failure.pl" -ForegroundColor Yellow
    exit
}

# Le JSON exact attendu par votre serveur (avec types corrigés)
$payload = @{
    percepts = @()
    beliefs = @{
        gridSize = 4
        certain_fluents = @{
            fat_hunter = @{
                c = @{ x = 1; y = 1 }
            }
            dir = @(
                @{ d = "east"; h = "hunter" }
            )
            visited = @()
        }
        percept_history = @()
    }
} | ConvertTo-Json -Depth 10

Write-Host "`n--- ENVOI DU JSON ---" -ForegroundColor Cyan
Write-Host $payload
Write-Host "---------------------`n"

try {
    $response = Invoke-RestMethod -Uri $url -Method Put -Body $payload -ContentType "application/json" -ErrorAction Stop
    Write-Host "--- RÉPONSE DU SERVEUR (SUCCESS) ---" -ForegroundColor Green
    Write-Host ($response | ConvertTo-Json -Depth 5)
}
catch {
    Write-Host "--- ERREUR DÉTECTÉE ---" -ForegroundColor Red
    Write-Host "Status Code: $($_.Exception.Response.StatusCode)" -ForegroundColor Yellow
    Write-Host "Message: $($_.Exception.Message)" -ForegroundColor Yellow
    
    # Lecture du corps de la réponse en cas d'erreur
    try {
        $response = $_.Exception.Response
        if ($response) {
            $stream = $response.GetResponseStream()
            $reader = New-Object System.IO.StreamReader($stream)
            $body = $reader.ReadToEnd()
            
            if ($body) {
                Write-Host "`n--- DÉTAILS DE L'ERREUR (Prolog/Serveur) ---" -ForegroundColor Yellow
                Write-Host $body
            }
        }
    } catch {
        Write-Host "Impossible de lire les détails de la réponse" -ForegroundColor Gray
    }
    
    exit 1
}