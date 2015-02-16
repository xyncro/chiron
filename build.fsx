#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"

open Fake

// Dirs

let tempDir = "./temp"
let srcDir = tempDir + "/src"

// Clean

Target "Clean" (fun _ ->
    CleanDirs [ tempDir ])

// Build

Target "Build" (fun _ ->
    !! "src/**/*.fsproj"
    |> MSBuildRelease srcDir "Build"
    |> Log "Build Source: ")

// Publish

Target "Publish" (fun _ ->
    NuGet (fun p ->
        { p with
              Authors = [ "Andrew Cherry" ]
              Project = "Chiron"
              OutputPath = tempDir
              WorkingDir = srcDir
              Version = "0.1.0-alpha"
              AccessKey = getBuildParamOrDefault "nuget_key" ""
              Publish = hasBuildParam "nuget_key"
              Dependencies =
                [ "Aether", GetPackageVersion "packages" "Aether"
                  "FParsec", GetPackageVersion "packages" "FParsec"
                  "FSharp.Core", GetPackageVersion "packages" "FSharp.Core" ]
              Files = 
                [ "Chiron.dll", Some "lib/net40", None
                  "Chiron.pdb", Some "lib/net40", None
                  "Chiron.xml", Some "lib/net40", None ] })
              "./nuget/Chiron.nuspec")

// Dependencies

"Clean"
    ==> "Build"
    ==> "Publish"

RunTargetOrDefault "Publish"
