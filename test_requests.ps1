# Test Request 1: Hunter at (1,2) with stench
$req1 = @{
    beliefs = @{
        certain_fluents = @{
            fat_hunter = @{ c = @{ x = 1; y = 2 } }
            dir = @(@{ d = "north"; h = @{ id = "hunter" } })
            visited = @(@{ to = @{ x = 1; y = 1 } })
        }
    }
    percepts = @("stench")
} | ConvertTo-Json -Depth 10

Write-Host "=== REQUEST 1: (1,2) with STENCH ===" 
$resp1 = Invoke-WebRequest -Uri "http://localhost:8081/action" -Method PUT -ContentType "application/json" -Body $req1
Write-Host $resp1.Content | ConvertFrom-Json | ConvertTo-Json -Depth 5

Start-Sleep -Seconds 1

# Test Request 2: Hunter at (2,1) with breeze (should trigger contradiction)
$req2 = @{
    beliefs = @{
        certain_fluents = @{
            fat_hunter = @{ c = @{ x = 2; y = 1 } }
            dir = @(@{ d = "east"; h = @{ id = "hunter" } })
            visited = @(@{ to = @{ x = 1; y = 1 } }, @{ to = @{ x = 1; y = 2 } })
        }
    }
    percepts = @("breeze")
} | ConvertTo-Json -Depth 10

Write-Host "`n=== REQUEST 2: (2,1) with BREEZE (should detect contradiction) ===" 
$resp2 = Invoke-WebRequest -Uri "http://localhost:8081/action" -Method PUT -ContentType "application/json" -Body $req2
Write-Host $resp2.Content | ConvertFrom-Json | ConvertTo-Json -Depth 5
