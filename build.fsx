#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"

open Fake

// Clean

Target "Clean" (fun _ ->
    CleanDirs [
        "bin"
        "temp" ])

// Build

Target "Build" (fun _ ->
    build (fun x ->
        { x with
            Properties =
                [ "Optimize",      environVarOrDefault "Build.Optimize"      "True"
                  "DebugSymbols",  environVarOrDefault "Build.DebugSymbols"  "True"
                  "Configuration", environVarOrDefault "Build.Configuration" "Release" ]
            Targets =
                [ "Build" ]
            Verbosity = Some Normal }) "Chiron.sln")

// Publish

Target "Publish" (fun _ ->
    NuGet (fun p ->
        { p with
              Authors =
                [ "Andrew Cherry"
                  "Michael Newton"
                  "Henrik Feldt"
                  "Marcus Griep" ]
              Project = "Chiron"
              Version = "5.1.0"
              OutputPath = "bin"
              AccessKey = getBuildParamOrDefault "nuget_key" ""
              Publish = hasBuildParam "nuget_key"
              Dependencies =
                [ "Aether", GetPackageVersion "packages" "Aether"
                  "FParsec", GetPackageVersion "packages" "FParsec"
                  "FSharp.Core", GetPackageVersion "packages" "FSharp.Core" ]
              Files = 
                [ @"..\src\Chiron\bin\Release\Chiron.dll", Some "lib/net45", None
                  @"..\src\Chiron\bin\Release\Chiron.pdb", Some "lib/net45", None
                  @"..\src\Chiron\bin\Release\Chiron.xml", Some "lib/net45", None ] })
              "./nuget/Chiron.nuspec")

// Dependencies

"Clean"
    ==> "Build"
#if MONO
#else
    ==> "Publish"
#endif

RunTargetOrDefault "Publish"
