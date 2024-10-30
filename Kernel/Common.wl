(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`Common`"]


(* ::Subsubsection:: *)
(*Utilities*)


ReinitializePalette
UpdatePalette
WorkspaceExistsQ


(* ::Subsubsection:: *)
(*Configuration*)


ConfigureWorkspaces
WorkspaceConfiguration
$DefaultWorkspace
$SaveFrequency
$BaseSaveDirectory


(* ::Subsubsection:: *)
(*WorkspaceManagement*)


$WorkspaceMetadata
WorkspaceSaveDirectory
WorkspaceNotebooksFile
workspaceExistQ
$FEPID
activeWorkspacePID
validateWorkspaceName
workspaceLastSaved
$WorkspaceStatus=None;
RecordWorkspaceMetadata


(* ::Subsubsection:: *)
(*GeneralWorkspace*)


$GeneralWorkspace
LoadGeneralWorkspace
PruneGeneralNotebookUUIDs
AddNotebookToGeneralList
$GeneralNotebookUUIDs


(* ::Subsubsection:: *)
(*SaveAndRecordNotebooks*)


SaveNotebook
RecordNotebookToWorkspace
RecordWorkspaceNotebooks
SaveAndRecordNotebooks
ReopenNotebooks
closeNotebooks
$ExcludedNotebooks


(* ::Subsubsection:: *)
(*Palette*)


ManageExcludedNotebooks
$PaletteDynamicBoxes
nameWorkspaceDialog


EndPackage[]
