nVar1 := 2 + 2
println(nVar1)
nVar2 := 10 - nVar1
println(nVar2)
nVar3 := -nVar1
println(nVar3)
nVar4 := ++nVar2
println(nVar4)
nVar1 /= 2

// Print the results
println(nVar1)
println(nVar2)
println(nVar3)
println(nVar4)

// Call errorlevel with a custom exit code
errorlevel(42)
