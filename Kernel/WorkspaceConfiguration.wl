(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`Configuration`"]


ConfigureWorkspaces
WorkspaceConfiguration
$DefaultWorkspace
$SaveFrequency
$BaseSaveDirectory

Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`"]


configpattern=KeyValuePattern[{
	"SaveFrequency"->_Quantity,
	"BaseSaveDirectory"->_?DirectoryQ,
	"DefaultWorkspaceName"->_String
}];


configdefaults=<|
	"SaveFrequency"->Quantity[300,"Seconds"],
	"BaseSaveDirectory"->FileNameJoin[{$HomeDirectory,"NotebookWorkspaces"}],
	"DefaultWorkspaceName"->"Primary Workspace"
|>;


$localworkspaceconfiguration="NotebookWorkspaces/WorkspaceConfiguration";
$WorkspaceConfiguration/:Set[$WorkspaceConfiguration,value_]:=(LocalSymbol[$localworkspaceconfiguration]=value)
$WorkspaceConfiguration:=LocalSymbol[$localworkspaceconfiguration]


WorkspaceConfiguration[]:=(
	$WorkspaceConfiguration/.{
		assoc:configpattern:>assoc,
		_:>initializeConfiguration[]
		}
	)

WorkspaceConfiguration[val_]:=Lookup[WorkspaceConfiguration[],val];


initializeConfiguration[]:=($WorkspaceConfiguration=configdefaults)


$SaveFrequency= WorkspaceConfiguration["SaveFrequency"];
$BaseSaveDirectory= WorkspaceConfiguration["BaseSaveDirectory"];
$DefaultWorkspace= WorkspaceConfiguration["DefaultWorkspaceName"];


ConfigureWorkspaces[]:=DialogInput[
	DynamicModule[
		{sec,dir,default,error},
		error=Spacer[1];
		{sec,dir,default}=WorkspaceConfiguration[{"SaveFrequency","BaseSaveDirectory","DefaultWorkspaceName"}];
	
		sec=ToString@sec;
	
		Grid[{
			{"Base save directory:",  FileNameSetter[Dynamic[dir],"Directory",Appearance->Button[Dynamic[dir]]]},
			{"Default workspace name:",  InputField[Dynamic[default],String]},
			{"Save frequency:",InputField[Dynamic[sec],String]},
			{Dynamic@error,SpanFromLeft},
			{
				Item[
					Row[{
						DefaultButton[error=Enclose[(
							sec=Confirm[Interpreter[Restricted["Quantity","Seconds"]][sec]];
							setConfig[{sec,dir,default}];
							DialogReturn[]),
							Style["Enter in seconds or specify a time unit.",Red]&]
							],
						CancelButton[]
						}],
					Alignment->Right
					],
				SpanFromLeft
				}
			},
			Alignment->{{Right,Left},Automatic}
		]
	],
	WindowTitle->"Configure Notebook Workspaces"
]

ConfigureWorkspaces[assoc_Association]/;MatchQ[assoc,configpattern]:=(
	setConfig[Lookup[assoc,{"SaveFrequency","BaseSaveDirectory","DefaultWorkspaceName"}]]
	)


setConfig[{sec_,dir_,default_}]:=(
	setSave[sec];
	setBaseDir[dir];
	setDefault[default];
		
	$WorkspaceConfiguration=<|
		"SaveFrequency"->sec,
		"BaseSaveDirectory"->dir,
		"DefaultWorkspaceName"->default
	|>;
	UpdateDynamicsUsing[$WorkspaceConfiguration];
	$WorkspaceConfiguration
)


setSave[time_Quantity]/;$SaveFrequency!=time:=(
	$SaveFrequency=time;
	SetWorkspace[$CurrentWorkspace,"Changing task frequency"];
)


setBaseDir[newdir_]/;$BaseSaveDirectory!=newdir:=Module[
	{workspaces=Append[Keys@WorkspaceMetadata[],$GeneralWorkspace],conflicts},
	
	conflicts=FileNames[workspaces,newdir];
	
(* go through the spaces, copying the directory if there's no conflict *)
(* if there's a conflict, rename the existing one and copy away *)	
	If[Length[conflicts]>0,
		SetDirectory[newdir];
		RenameFile[#,#<>"-backup"]&/@conflicts;
		SetDirectory[];
		];
	
	CopyDirectory[FileNameJoin[{$BaseSaveDirectory,#}],FileNameJoin[{newdir,#}]]&/@workspaces;
	
	$BaseSaveDirectory=newdir;
];
(* copy the contents of the existing base dir, if any
	do we need to check the new destination is safe first? *)


setDefault[default_String]:=(	
	If[!KeyExistsQ[WorkspaceMetadata[],default],
		$WorkspaceMetadata=Append[WorkspaceMetadata[],default-><|"FEPID"->None,"Event"->"InitializeNewDefaultWorkspace","Timestamp"->Now|>]];
	
	$DefaultWorkspace=default
	)


End[];

EndPackage[]
