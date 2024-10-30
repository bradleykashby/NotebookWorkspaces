(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "BradleyAshby/NotebookWorkspaces",
    "Description" -> "Workspaces for auto-saving and quickly switching between sets of notebooks",
    "Creator" -> "Bradley Ashby",
    "License" -> "MIT",
    "PublisherID" -> "BradleyAshby",
    "Version" -> "1.2.0",
    "WolframVersion" -> "13.1+",
    "PrimaryContext" -> "BradleyAshby`NotebookWorkspaces`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "HiddenImport" -> "BradleyAshby`NotebookWorkspaces`WorkspaceManagement`",
        "Context" -> {
          {
            "BradleyAshby`NotebookWorkspaces`",
            "NotebookWorkspaces.wl"
          },
          {
            "BradleyAshby`NotebookWorkspaces`Common`",
            "Common.wl"
          },
          {
            "BradleyAshby`NotebookWorkspaces`Utilities`",
            "Utilities.wl"
          },
          {
            "BradleyAshby`NotebookWorkspaces`Configuration`",
            "WorkspaceConfiguration.wl"
          },
          {
            "BradleyAshby`NotebookWorkspaces`WorkspaceManagement`",
            "WorkspaceManagement.wl"
          },
          {
            "BradleyAshby`NotebookWorkspaces`SaveAndRecordNotebooks`",
            "SaveAndRecordNotebooks.wl"
          },
          {
            "BradleyAshby`NotebookWorkspaces`GeneralWorkspace`",
            "GeneralWorkspace.wl"
          },
          {
            "BradleyAshby`NotebookWorkspaces`Palette`",
            "Palette.wl"
          }
        },
        "Symbols" -> {
          "BradleyAshby`NotebookWorkspaces`SaveWorkspace",
          "BradleyAshby`NotebookWorkspaces`LoadWorkspace",
          "BradleyAshby`NotebookWorkspaces`SaveWorkspaceAs",
          "BradleyAshby`NotebookWorkspaces`CloseWorkspace",
          "BradleyAshby`NotebookWorkspaces`CleanWorkspace",
          "BradleyAshby`NotebookWorkspaces`RemoveWorkspace",
          "BradleyAshby`NotebookWorkspaces`$CurrentWorkspace",
          "BradleyAshby`NotebookWorkspaces`ExcludedNotebooks",
          "BradleyAshby`NotebookWorkspaces`WorkspaceMetadata",
          "BradleyAshby`NotebookWorkspaces`AddToExcludedNotebooks",
          "BradleyAshby`NotebookWorkspaces`RemoveFromExcludedNotebooks",
          "BradleyAshby`NotebookWorkspaces`GeneralNotebooks",
          "BradleyAshby`NotebookWorkspaces`AddNotebookToGeneral",
          "BradleyAshby`NotebookWorkspaces`RemoveNotebookFromGeneral",
          "BradleyAshby`NotebookWorkspaces`AddNotebookToWorkspace"
        }
      },
      {"FrontEnd"},
      {"Documentation", "Language" -> "English"}
    }
  |>
]
