(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`"]


SetWorkspace
SaveWorkspace
LoadWorkspace
SaveWorkspaceAs
CloseWorkspace
CleanWorkspace
RemoveWorkspace
$CurrentWorkspace
ExcludedNotebooks
WorkspaceMetadata
AddToExcludedNotebooks
RemoveFromExcludedNotebooks
AddNotebookToGeneral
RemoveNotebookFromGeneral
GeneralNotebooks
RebuildWorkspaceMetadata
AddNotebookToWorkspace

Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`Configuration`"]
Needs["BradleyAshby`NotebookWorkspaces`WorkspaceManagement`"]
Needs["BradleyAshby`NotebookWorkspaces`GeneralWorkspace`"]
Needs["BradleyAshby`NotebookWorkspaces`SaveAndRecordNotebooks`"]
Needs["BradleyAshby`NotebookWorkspaces`Palette`"]


End[];

EndPackage[]
