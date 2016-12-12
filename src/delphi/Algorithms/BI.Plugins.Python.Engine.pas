(**************************************************************************)
(*                                                                        *)
(*  SPECIAL:                                                              *)
(*  This unit corresponds to Python4Delphi "PythonEngine.pas"             *)
(*                                                                        *)
(*  It has been renamed and slightly adapted to integrate it in TeeBI     *)
(*  avoiding the dependency to Python4Delph full library.                 *)
(*                                                                        *)
(*  The only change is merging the Definition.inc include file and the    *)
(*  MethodCallback.pas unit.                                              *)
(**************************************************************************)

(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PythonEngine'     Copyright (c) 1997                    *)
(*                                                                        *)
(* Version: 3.0                     Dr. Dietmar Budelsky                  *)
(* Sub-Version: 0.33                dbudelsky@web.de                      *)
(*                                  Germany                               *)
(*                                                                        *)
(*                                  Morgan Martinet                       *)
(*                                  4723 rue Brebeuf                      *)
(*                                  H2J 3L2 MONTREAL (QC)                 *)
(*                                  CANADA                                *)
(*                                  e-mail: p4d@mmm-experts.com           *)
(*                                                                        *)
(*  look at the project page at: http://python4Delphi.googlecode.com/     *)
(**************************************************************************)
(*  Functionality:  Delphi Components that provide an interface to the    *)
(*                  Python language (see python.txt for more infos on     *)
(*                  Python itself).                                       *)
(*                                                                        *)
(**************************************************************************)
(*  Contributors:                                                         *)
(*      Grzegorz Makarewicz (mak@mikroplan.com.pl)                        *)
(*      Andrew Robinson (andy@hps1.demon.co.uk)                           *)
(*      Mark Watts(mark_watts@hotmail.com)                                *)
(*      Olivier Deckmyn (olivier.deckmyn@mail.dotcom.fr)                  *)
(*      Sigve Tjora (public@tjora.no)                                     *)
(*      Mark Derricutt (mark@talios.com)                                  *)
(*      Igor E. Poteryaev (jah@mail.ru)                                   *)
(*      Yuri Filimonov (fil65@mail.ru)                                    *)
(*      Stefan Hoffmeister (Stefan.Hoffmeister@Econos.de)                 *)
(*      Michiel du Toit (micdutoit@hsbfn.com) - Lazarus Port              *)
(*      Chris Nicolai (nicolaitanes@gmail.com)                            *)
(*      Kiriakos Vlahos (kvlahos@london.edu)                              *)
(*      Andrey Gruzdev      (andrey.gruzdev@gmail.com)                    *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(* Dr. Dietmar Budelsky, 1997-11-17                                       *)
(**************************************************************************)

// MERGED: {$I Definition.Inc}

(**************************************************************************)
(*                                                                        *)
(* Module:   'Definition.Inc'          Copyright (c) 1998                 *)
(*                                                                        *)
(* Version: 1.5                        Dr. Dietmar Budelsky               *)
(*                                     budelsky@ibs.bm.eunet.de           *)
(*                                     IBS Schillings GmbH & Co KG        *)
(*                                     Ein Unternehmen der KROHNE-Gruppe  *)
(*                                     Heisenbergstr. 18                  *)
(*                                     50169 Kerpen-Türnich               *)
(*                                     Phone: (49)22 37/97 44-0           *)
(*                                                                        *)
(**************************************************************************)
(*  Changes for Delphi 4 or higher are made for Compilers higher          *)
(*  than Version 10.0.                                                     *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free, as long as this  *)
(* header and its copyright text is intact.                               *)
(* Dr. Dietmar Budelsky, 1998-01-07                                       *)
(**************************************************************************)
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////
// Select a Python version by commenting out the corresponding line, or
// modify the default Python version below, in the Python versions section.
/////////////////////////////////////////////////////////////////////////////

//{$DEFINE PYTHON23}
//{$DEFINE PYTHON24}
//{$DEFINE PYTHON25}
//{$DEFINE PYTHON26}
//{$DEFINE PYTHON27}
//{$DEFINE PYTHON30}
//{$DEFINE PYTHON31}
//{$DEFINE PYTHON32}
//{$DEFINE PYTHON33}
//{$DEFINE PYTHON34}
//{$DEFINE PYTHON35}

/////////////////////////////////////////////////////////////////////////////
// Python for Delphi settings. Comment out the setting you want to enable.
/////////////////////////////////////////////////////////////////////////////

{$DEFINE PREFER_UNICODE} // this will convert a variant containing an OleStr to a UniCode Python string.

/////////////////////////////////////////////////////////////////////////////
// OS symbols. Note that MSWINDOWS is defined from Delphi6/Kylix.
/////////////////////////////////////////////////////////////////////////////

{$IFNDEF MSWINDOWS}
  {$IFDEF WIN32}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////
// Delphi, C++ Builder and Kylix versions
/////////////////////////////////////////////////////////////////////////////

{$IFDEF VER90} // Delphi 2.0
  {$DEFINE DELPHI2}
{$ENDIF}
{$IFDEF VER93} // C++ Builder 1.0
  {$DEFINE CBUILDER1}
{$ENDIF}
{$IFDEF VER100} // Delphi 3.0
  {$DEFINE DELPHI3}
{$ENDIF}
{$IFDEF VER110} // C++ Builder 3.0
  {$DEFINE CBUILDER3}
  {$DEFINE DELPHI3_OR_HIGHER}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER120} // Delphi 4.0
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI4_OR_HIGHER}
{$ENDIF}
{$IFDEF VER125} // C++ Builder 4.0
  {$DEFINE CBUILDER4}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER130} // Delphi 5.0
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$IFDEF BCB}  { Borland C++Builder 5.0 }
    {$DEFINE CBUILDER3}
    {$DEFINE CBUILDER4}
    {$DEFINE CBUILDER5}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}
{$IFDEF VER140} // Delphi 6.0
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$IFDEF BCB}  { Borland C++Builder 6.0 }
    {$DEFINE CBUILDER3}
    {$DEFINE CBUILDER4}
    {$DEFINE CBUILDER5}
    {$DEFINE CBUILDER6}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}
{$IFDEF VER150} // Delphi 7.0
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
{$ENDIF}
{$IFDEF VER160} // Delphi 8
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
{$ENDIF}
{$IFDEF VER170} // Delphi 2005
  {$DEFINE DELPHI2005}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
{$ENDIF}
{$IFDEF VER180} // Delphi 2006
  {$DEFINE DELPHI2006}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
{$ENDIF}
{$IFDEF VER190} // Delphi 2007
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
{$ENDIF}
{$IFDEF VER200} // Delphi 2009
  {$DEFINE DELPHI2009}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
{$ENDIF}
{$IFDEF VER210} // Delphi 2010
  {$DEFINE DELPHI2010}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
{$ENDIF}
{$IFDEF VER220} // Delphi XE
  {$DEFINE DELPHIXE}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
{$ENDIF}
{$IFDEF VER230} // Delphi XE2
  {$DEFINE DELPHIXE2}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
{$ENDIF}
{$IFDEF VER240} // Delphi XE3
  {$DEFINE DELPHIXE3}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
{$ENDIF}
{$IFDEF VER250} // Delphi XE4
  {$DEFINE DELPHIXE4}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
{$ENDIF}
{$IFDEF VER260} // Delphi XE5
  {$DEFINE DELPHIXE5}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
{$ENDIF}
{$IFDEF VER270} // Delphi XE6
  {$DEFINE DELPHIXE6}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
{$ENDIF}
{$IFDEF VER280} // Delphi XE7
  {$DEFINE DELPHIXE7}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
{$ENDIF}
{$IFDEF VER290} // Delphi XE8
  {$DEFINE DELPHIXE8}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
{$ENDIF}
{$IFDEF VER300} // Delphi 10 Seattle
  {$DEFINE DELPHIX}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
  {$DEFINE DELPHIX_OR_HIGHER}
{$ENDIF}
{$IFDEF VER310} // Delphi 10.1 London
  {$DEFINE DELPHIX}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
  {$DEFINE DELPHIX_OR_HIGHER}
  {$DEFINE DELPHIX01_OR_HIGHER}
{$ENDIF}
/////////////////////////////////////////////////////////////////////////////
// Python versions
/////////////////////////////////////////////////////////////////////////////

// Here we select a default Python version, if no version was explicitely specified.
// Note that the installer will let the user specify its default Python version, and
// thus will edit this file.

{$IFNDEF PYTHON35}
  {$IFNDEF PYTHON34}
    {$IFNDEF PYTHON33}
      {$IFNDEF PYTHON32}
        {$IFNDEF PYTHON31}
          {$IFNDEF PYTHON30}
            {$IFNDEF PYTHON27}
              {$IFNDEF PYTHON26}
                {$IFNDEF PYTHON25}
                  {$IFNDEF PYTHON24}
                    {$IFNDEF PYTHON23}
              {---<START OF DEFAULT PYTHON VERSION>---}
                  {$DEFINE PYTHON27}
              {---<END OF DEFAULT PYTHON VERSION>---}
                    {$ENDIF}
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


{$IFDEF PYTHON23}
  {$DEFINE PYTHON23_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON24}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON25}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON26}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON27}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON27_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON30}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON31}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON32}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
  {$DEFINE PYTHON32_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON33}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
  {$DEFINE PYTHON32_OR_HIGHER}
  {$DEFINE PYTHON33_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON34}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
  {$DEFINE PYTHON32_OR_HIGHER}
  {$DEFINE PYTHON33_OR_HIGHER}
  {$DEFINE PYTHON34_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON35}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
  {$DEFINE PYTHON32_OR_HIGHER}
  {$DEFINE PYTHON33_OR_HIGHER}
  {$DEFINE PYTHON34_OR_HIGHER}
  {$DEFINE PYTHON35_OR_HIGHER}
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////
// Misc
/////////////////////////////////////////////////////////////////////////////

{$IFDEF WIN32}
  {$DEFINE CPUX86}
{$ENDIF}

{$IFDEF FPC}
//  {$MODE DELPHI}
  {$IFDEF CPU64}
    {$DEFINE CPUX64}
  {$ENDIF CPU64}
  {$IFDEF CPU32}
    {$DEFINE CPUX86}
  {$ENDIF CPU32}
  {$IFDEF DARWIN}
    {$DEFINE MACOS}
    {$DEFINE ALIGN_STACK}
    {$IFDEF CPU32}
      {$DEFINE MACOS32}
    {$ENDIF CPU32}
  {$ENDIF DARWIN}
{$ENDIF FPC}



unit BI.Plugins.Python.Engine;

{ TODO -oMMM : implement tp_as_buffer slot }
{ TODO -oMMM : implement Attribute descriptor and subclassing stuff }

{$IFNDEF FPC}
  {$IFNDEF DELPHI7_OR_HIGHER}
      Error Delphi 7 or higher is required!
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  Posix.PThread,
{$ENDIF}
{$IFDEF LINUX}
  Types,
  Libc,
{$ENDIF}
  Classes,
  SysUtils,

{$IFDEF DELPHIXE4_OR_HIGHER}
  {$IFNDEF IOS}
  {$IFNDEF ANDROID}
  {$DEFINE USEANSISTRPAS}
  System.AnsiStrings,
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

  SyncObjs,
  Variants,
{$IFDEF DELPHI2005_OR_HIGHER}
  System.Types;
{$IFNDEF UNICODE}
  WideStrings;
{$ENDIF}
{$ELSE}
  TinyWideStrings;
{$ENDIF}
// MERGED:  MethodCallBack;

//#######################################################
//##                                                   ##
//##           PYTHON specific constants               ##
//##                                                   ##
//#######################################################

type
{$IFNDEF UNICODE}
  UnicodeString = WideString;
  TUnicodeStringList = TWideStringList;
{$ELSE}
  TUnicodeStringList = TStringList;
{$ENDIF}

{$IFNDEF FPC}
  {$IF CompilerVersion < 21}
    NativeInt = integer;
    NativeUInt = Cardinal;
  {$IFEND}
  PNativeInt = ^NativeInt;
{$ELSE}
  {$IF DEFINED(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20500)}
  {$ELSE}
    NativeInt = integer;
    NativeUInt = Cardinal;
  {$IFEND}
  PNativeInt = ^NativeInt;
{$ENDIF}



  TPythonVersionProp = packed record
    DllName      : String;
    RegVersion   : String;
    APIVersion   : Integer;
    CanUseLatest : Boolean;
  end;
const
{$IFDEF MSWINDOWS}
  PYTHON_KNOWN_VERSIONS: array[1..11] of TPythonVersionProp =
  ( (DllName: 'python23.dll'; RegVersion: '2.3'; APIVersion: 1012; CanUseLatest: True),
    (DllName: 'python24.dll'; RegVersion: '2.4'; APIVersion: 1012; CanUseLatest: True),
    (DllName: 'python25.dll'; RegVersion: '2.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python26.dll'; RegVersion: '2.6'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python27.dll'; RegVersion: '2.7'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python30.dll'; RegVersion: '3.0'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python31.dll'; RegVersion: '3.1'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python32.dll'; RegVersion: '3.2'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python33.dll'; RegVersion: '3.3'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python34.dll'; RegVersion: '3.4'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python35.dll'; RegVersion: '3.5'; APIVersion: 1013; CanUseLatest: True) );
{$ENDIF}
{$IFDEF LINUX}
  PYTHON_KNOWN_VERSIONS: array[1..11] of TPythonVersionProp =
  ( (DllName: 'libpython2.3.so'; RegVersion: '2.3'; APIVersion: 1012; CanUseLatest: True),
    (DllName: 'libpython2.4.so'; RegVersion: '2.4'; APIVersion: 1012; CanUseLatest: True),
    (DllName: 'libpython2.5.so'; RegVersion: '2.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython2.6.so'; RegVersion: '2.6'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython2.7.so'; RegVersion: '2.7'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.0.so'; RegVersion: '3.0'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.1.so'; RegVersion: '3.1'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.2.so'; RegVersion: '3.2'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.3.so'; RegVersion: '3.3'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.4.so'; RegVersion: '3.4'; APIVersion: 1013; CanUseLatest: True) );
    (DllName: 'libpython3.5.so'; RegVersion: '3.5'; APIVersion: 1013; CanUseLatest: True) );
{$ENDIF}
{$IFDEF MACOS}
  PYTHON_KNOWN_VERSIONS: array[1..1] of TPythonVersionProp =
  ( (DllName: 'libpython3.4.dylib'; RegVersion: '3.4'; APIVersion: 1013; CanUseLatest: True) );
  COMPILED_FOR_PYTHON_VERSION_INDEX = 1;
{$ELSE}
{$IFDEF PYTHON23}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 1;
{$ENDIF}
{$IFDEF PYTHON24}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 2;
{$ENDIF}
{$IFDEF PYTHON25}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 3;
{$ENDIF}
{$IFDEF PYTHON26}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 4;
{$ENDIF}
{$IFDEF PYTHON27}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 5;
{$ENDIF}
{$IFDEF PYTHON30}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 6;
{$ENDIF}
{$IFDEF PYTHON31}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 7;
{$ENDIF}
{$IFDEF PYTHON32}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 8;
{$ENDIF}
{$IFDEF PYTHON33}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 9;
{$ENDIF}
{$IFDEF PYTHON34}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 10;
{$ENDIF}
{$IFDEF PYTHON35}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 11;
{$ENDIF}
{$ENDIF}

  PYT_METHOD_BUFFER_INCREASE = 10;
  PYT_MEMBER_BUFFER_INCREASE = 10;
  PYT_GETSET_BUFFER_INCREASE = 10;

  METH_VARARGS  = $0001;
  METH_KEYWORDS = $0002;

  // Masks for the co_flags field of PyCodeObject
  CO_OPTIMIZED   = $0001;
  CO_NEWLOCALS   = $0002;
  CO_VARARGS     = $0004;
  CO_VARKEYWORDS = $0008;

  // Rich comparison opcodes introduced in version 2.1
  Py_LT = 0;
  Py_LE = 1;
  Py_EQ = 2;
  Py_NE = 3;
  Py_GT = 4;
  Py_GE = 5;
type
  // Delphi equivalent used by TPyObject
  TRichComparisonOpcode = (pyLT, pyLE, pyEQ, pyNE, pyGT, pyGE);
const
{Type flags (tp_flags) introduced in version 2.0

These flags are used to extend the type structure in a backwards-compatible
fashion. Extensions can use the flags to indicate (and test) when a given
type structure contains a new feature. The Python core will use these when
introducing new functionality between major revisions (to avoid mid-version
changes in the PYTHON_API_VERSION).

Arbitration of the flag bit positions will need to be coordinated among
all extension writers who publically release their extensions (this will
be fewer than you might expect!)..

Python 1.5.2 introduced the bf_getcharbuffer slot into PyBufferProcs.

Type definitions should use Py_TPFLAGS_DEFAULT for their tp_flags value.

Code can use PyType_HasFeature(type_ob, flag_value) to test whether the
given type object has a specified feature.
}

// PyBufferProcs contains bf_getcharbuffer
  Py_TPFLAGS_HAVE_GETCHARBUFFER = (1 shl 0);

// PySequenceMethods contains sq_contains
  Py_TPFLAGS_HAVE_SEQUENCE_IN = (1 shl 1);

// Objects which participate in garbage collection (see objimp.h)
  Py_TPFLAGS_GC = (1 shl 2);

// PySequenceMethods and PyNumberMethods contain in-place operators
  Py_TPFLAGS_HAVE_INPLACEOPS = (1 shl 3);

// PyNumberMethods do their own coercion */
  Py_TPFLAGS_CHECKTYPES = (1 shl 4);

  Py_TPFLAGS_HAVE_RICHCOMPARE = (1 shl 5);

// Objects which are weakly referencable if their tp_weaklistoffset is >0
// XXX Should this have the same value as Py_TPFLAGS_HAVE_RICHCOMPARE?
// These both indicate a feature that appeared in the same alpha release.

  Py_TPFLAGS_HAVE_WEAKREFS = (1 shl 6);

// tp_iter is defined
  Py_TPFLAGS_HAVE_ITER = (1 shl 7);

// New members introduced by Python 2.2 exist
  Py_TPFLAGS_HAVE_CLASS = (1 shl 8);

// Set if the type object is dynamically allocated
  Py_TPFLAGS_HEAPTYPE = (1 shl 9);

// Set if the type allows subclassing
  Py_TPFLAGS_BASETYPE = (1 shl 10);

// Set if the type is 'ready' -- fully initialized
  Py_TPFLAGS_READY = (1 shl 12);

// Set while the type is being 'readied', to prevent recursive ready calls
  Py_TPFLAGS_READYING = (1 shl 13);

// Objects support garbage collection (see objimp.h)
  Py_TPFLAGS_HAVE_GC = (1 shl 14);

  Py_TPFLAGS_DEFAULT  =      Py_TPFLAGS_HAVE_GETCHARBUFFER
                             or Py_TPFLAGS_HAVE_SEQUENCE_IN
                             or Py_TPFLAGS_HAVE_INPLACEOPS
                             or Py_TPFLAGS_HAVE_RICHCOMPARE
                             or Py_TPFLAGS_HAVE_WEAKREFS
                             or Py_TPFLAGS_HAVE_ITER
                             or Py_TPFLAGS_HAVE_CLASS
                             or Py_TPFLAGS_BASETYPE
                             ;

// See function PyType_HasFeature below for testing the flags.

// Delphi equivalent used by TPythonType
type
  TPFlag = (tpfHaveGetCharBuffer, tpfHaveSequenceIn, tpfGC, tpfHaveInplaceOps,
            tpfCheckTypes, tpfHaveRichCompare, tpfHaveWeakRefs
            ,tpfHaveIter, tpfHaveClass, tpfHeapType, tpfBaseType, tpfReady, tpfReadying, tpfHaveGC
            );
  TPFlags = set of TPFlag;
const
  TPFLAGS_DEFAULT = [tpfHaveGetCharBuffer, tpfHaveSequenceIn, tpfHaveInplaceOps,
                     tpfHaveRichCompare, tpfHaveWeakRefs, tpfHaveIter,
                     tpfHaveClass, tpfBaseType
                    ];
//-------  Python opcodes  ----------//
Const
   single_input                     = 256;
   file_input                       = 257;
   eval_input                       = 258;
   p4d_funcdef                      = 259;
   p4d_parameters                   = 260;
   p4d_varargslist                  = 261;
   p4d_fpdef                        = 262;
   p4d_fplist                       = 263;
   p4d_stmt                         = 264;
   p4d_simple_stmt                  = 265;
   p4d_small_stmt                   = 266;
   p4d_expr_stmt                    = 267;
   p4d_augassign                    = 268;
   p4d_print_stmt                   = 269;
   p4d_del_stmt                     = 270;
   p4d_pass_stmt                    = 271;
   p4d_flow_stmt                    = 272;
   p4d_break_stmt                   = 273;
   p4d_continue_stmt                = 274;
   p4d_return_stmt                  = 275;
   p4d_raise_stmt                   = 276;
   p4d_import_stmt                  = 277;
   p4d_import_as_name               = 278;
   p4d_dotted_as_name               = 279;
   p4d_dotted_name                  = 280;
   p4d_global_stmt                  = 281;
   p4d_exec_stmt                    = 282;
   p4d_assert_stmt                  = 283;
   p4d_compound_stmt                = 284;
   p4d_if_stmt                      = 285;
   p4d_while_stmt                   = 286;
   p4d_for_stmt                     = 287;
   p4d_try_stmt                     = 288;
   p4d_except_clause                = 289;
   p4d_suite                        = 290;
   p4d_test                         = 291;
   p4d_and_test                     = 291;
   p4d_not_test                     = 293;
   p4d_comparison                   = 294;
   p4d_comp_op                      = 295;
   p4d_expr                         = 296;
   p4d_xor_expr                     = 297;
   p4d_and_expr                     = 298;
   p4d_shift_expr                   = 299;
   p4d_arith_expr                   = 300;
   p4d_term                         = 301;
   p4d_factor                       = 302;
   p4d_power                        = 303;
   p4d_atom                         = 304;
   p4d_listmaker                    = 305;
   p4d_lambdef                      = 306;
   p4d_trailer                      = 307;
   p4d_subscriptlist                = 308;
   p4d_subscript                    = 309;
   p4d_sliceop                      = 310;
   p4d_exprlist                     = 311;
   p4d_testlist                     = 312;
   p4d_dictmaker                    = 313;
   p4d_classdef                     = 314;
   p4d_arglist                      = 315;
   p4d_argument                     = 316;
   p4d_list_iter                    = 317;
   p4d_list_for                     = 318;
   p4d_list_if                      = 319;

  // structmember.h
const
//* Types */
  T_SHORT                       = 0;
  T_INT                         = 1;
  T_LONG                        = 2;
  T_FLOAT                       = 3;
  T_DOUBLE                      = 4;
  T_STRING                      = 5;
  T_OBJECT                      = 6;
//* XXX the ordering here is weird for binary compatibility */
  T_CHAR                        = 7;	//* 1-character string */
  T_BYTE                        = 8;	//* 8-bit signed int */
//* unsigned variants: */
  T_UBYTE                       = 9;
  T_USHORT                      = 10;
  T_UINT                        = 11;
  T_ULONG                       = 12;

//* Added by Jack: strings contained in the structure */
  T_STRING_INPLACE= 13;

  T_OBJECT_EX                   = 16;{* Like T_OBJECT, but raises AttributeError
                                        when the value is NULL, instead of
                                        converting to None. *}

//* Flags */
  READONLY                      = 1;
  RO                            = READONLY;		//* Shorthand */
  READ_RESTRICTED               = 2;

  // Workaround for C++ *.hpp autogenerated unit, conflict with "WRITE_RESTRICTED"
  //https://quality.embarcadero.com/browse/RSP-15030
  // WRITE_RESTRICTED              = 4;

  {$EXTERNALSYM WRITE_RESTRICTED}
  WRITE_RESTRICTED              = 4;
  RESTRICTED                    = (READ_RESTRICTED or WRITE_RESTRICTED);

type
  TPyMemberType = (mtShort, mtInt, mtLong, mtFloat, mtDouble, mtString, mtObject,
                   mtChar, mtByte, mtUByte, mtUShort, mtUInt, mtULong,
                   mtStringInplace, mtObjectEx);
  TPyMemberFlag = (mfDefault, mfReadOnly, mfReadRestricted, mfWriteRestricted, mfRestricted);

//#######################################################
//##                                                   ##
//##           Non-Python specific constants           ##
//##                                                   ##
//#######################################################

const
  ErrInit         = -300;
  CR              = #13;
  LF              = #10;
  TAB             = #09;
  CRLF            = CR+LF;



//#######################################################
//##                                                   ##
//##    Global declarations, nothing Python specific   ##
//##                                                   ##
//#######################################################

type
   TPAnsiChar     = array[0..16000] of PAnsiChar;
   TPWideChar = array[0..16000] of PWideChar;
   PPAnsiChar     = ^TPAnsiChar;
   PPWideChar = ^TPWideChar;
   PInt	      = ^Integer;
   PDouble    = ^Double;
   PFloat     = ^Real;
   PLong      = ^LongInt;
   PShort     = ^ShortInt;


//#######################################################
//##                                                   ##
//##            Python specific interface              ##
//##                                                   ##
//#######################################################

type
  PP_frozen	    = ^P_frozen;
  P_frozen	    = ^_frozen;
  PPyObject	    = ^PyObject;
  PPPyObject	    = ^PPyObject;
  PPPPyObject	    = ^PPPyObject;
  PPyIntObject	    = ^PyIntObject;
  PPyTypeObject     = ^PyTypeObject;
  PPySliceObject    = ^PySliceObject;

  AtExitProc        = procedure;
  PyCFunction       = function( self, args:PPyObject): PPyObject; cdecl;
  PyCFunctionWithKW = function( self, args, keywords:PPyObject): PPyObject; cdecl;

  unaryfunc         = function( ob1 : PPyObject): PPyObject; cdecl;
  binaryfunc        = function( ob1,ob2 : PPyObject): PPyObject; cdecl;
  ternaryfunc       = function( ob1,ob2,ob3 : PPyObject): PPyObject; cdecl;
  inquiry           = function( ob1 : PPyObject): integer; cdecl;
  lenfunc           = function( ob1 : PPyObject): NativeInt; cdecl;
  coercion          = function( ob1,ob2 : PPPyObject): integer; cdecl;
  ssizeargfunc      = function( ob1 : PPyObject; i: NativeInt): PPyObject; cdecl;
  ssizessizeargfunc = function( ob1 : PPyObject; i1, i2: NativeInt):
                                PPyObject; cdecl;
  ssizeobjargproc   = function( ob1 : PPyObject; i: NativeInt; ob2 : PPyObject):
                                integer; cdecl;
  ssizessizeobjargproc = function( ob1: PPyObject; i1, i2: NativeInt;
                                ob2: PPyObject): integer; cdecl;
  objobjargproc     = function( ob1,ob2,ob3 : PPyObject): integer; cdecl;

  pydestructor      = procedure(ob: PPyObject); cdecl;
  printfunc         = function( ob: PPyObject; var f: file; i: integer): integer; cdecl;
  getattrfunc       = function( ob1: PPyObject; name: PAnsiChar): PPyObject; cdecl;
  setattrfunc       = function( ob1: PPyObject; name: PAnsiChar; ob2: PPyObject): integer; cdecl;
  cmpfunc           = function( ob1,ob2: PPyObject): integer; cdecl;
  reprfunc          = function( ob: PPyObject): PPyObject; cdecl;
  hashfunc          = function( ob: PPyObject): NativeInt; cdecl; // !! in 2.x it is still a LongInt
  getattrofunc      = function( ob1,ob2: PPyObject): PPyObject; cdecl;
  setattrofunc      = function( ob1,ob2,ob3: PPyObject): integer; cdecl;

/// jah 29-sep-2000 : updated for python 2.0
///                   added from object.h
  getreadbufferproc = function ( ob1: PPyObject; i: NativeInt; ptr: Pointer): NativeInt; cdecl;
  getwritebufferproc= function ( ob1: PPyObject; i: NativeInt; ptr: Pointer): NativeInt; cdecl;
  getsegcountproc   = function ( ob1: PPyObject; i: NativeInt): NativeInt; cdecl;
  getcharbufferproc = function ( ob1: PPyObject; i: NativeInt; const pstr: PAnsiChar): NativeInt; cdecl;
  objobjproc        = function ( ob1, ob2: PPyObject): integer; cdecl;
  visitproc         = function ( ob1: PPyObject; ptr: Pointer): integer; cdecl;
  traverseproc      = function ( ob1: PPyObject; proc: visitproc; ptr: Pointer): integer; cdecl;

  richcmpfunc       = function ( ob1, ob2 : PPyObject; i : Integer) : PPyObject; cdecl;
  getiterfunc       = function ( ob1 : PPyObject) : PPyObject; cdecl;
  iternextfunc      = function ( ob1 : PPyObject) : PPyObject; cdecl;
  descrgetfunc      = function ( ob1, ob2, ob3 : PPyObject) : PPyObject; cdecl;
  descrsetfunc      = function ( ob1, ob2, ob3 : PPyObject) : Integer; cdecl;
  initproc          = function ( self, args, kwds : PPyObject) : Integer; cdecl;
  newfunc           = function ( subtype: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;
  allocfunc         = function ( self: PPyTypeObject; nitems : NativeInt) : PPyObject; cdecl;

  PyNumberMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     nb_add           : binaryfunc;
     nb_substract     : binaryfunc;
     nb_multiply      : binaryfunc;
     nb_divide        : binaryfunc;
     nb_remainder     : binaryfunc;
     nb_divmod        : binaryfunc;
     nb_power         : ternaryfunc;
     nb_negative      : unaryfunc;
     nb_positive      : unaryfunc;
     nb_absolute      : unaryfunc;
     nb_nonzero       : inquiry;
     nb_invert        : unaryfunc;
     nb_lshift        : binaryfunc;
     nb_rshift        : binaryfunc;
     nb_and           : binaryfunc;
     nb_xor           : binaryfunc;
     nb_or            : binaryfunc;
     nb_coerce        : coercion;
     nb_int           : unaryfunc;
     nb_long          : unaryfunc;
     nb_float         : unaryfunc;
     nb_oct           : unaryfunc;
     nb_hex           : unaryfunc;

/// jah 29-sep-2000 : updated for python 2.0
///                   added from .h
     nb_inplace_add       : binaryfunc;
     nb_inplace_subtract  : binaryfunc;
     nb_inplace_multiply  : binaryfunc;
     nb_inplace_divide    : binaryfunc;
     nb_inplace_remainder : binaryfunc;
     nb_inplace_power     : ternaryfunc;
     nb_inplace_lshift    : binaryfunc;
     nb_inplace_rshift    : binaryfunc;
     nb_inplace_and       : binaryfunc;
     nb_inplace_xor       : binaryfunc;
     nb_inplace_or        : binaryfunc;

     // Added in release 2.2
     // The following require the Py_TPFLAGS_HAVE_CLASS flag
     nb_floor_divide         : binaryfunc;
     nb_true_divide          : binaryfunc;
     nb_inplace_floor_divide : binaryfunc;
     nb_inplace_true_divide  : binaryfunc;
  end;
  PPyNumberMethods = ^PyNumberMethods;

  PySequenceMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     sq_length    : lenfunc;
     sq_concat    : binaryfunc;
     sq_repeat    : ssizeargfunc;
     sq_item      : ssizeargfunc;
     sq_slice     : ssizessizeargfunc;
     sq_ass_item  : ssizeobjargproc;
     sq_ass_slice : ssizessizeobjargproc;

/// jah 29-sep-2000 : updated for python 2.0
///                   added from .h
     sq_contains        : objobjproc;
     sq_inplace_concat  : binaryfunc;
     sq_inplace_repeat  : ssizeargfunc;
  end;
  PPySequenceMethods = ^PySequenceMethods;

  PyMappingMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     mp_length	      : lenfunc;
     mp_subscript     : binaryfunc;
     mp_ass_subscript : objobjargproc;
  end;
  PPyMappingMethods = ^PyMappingMethods;

/// jah 29-sep-2000 : updated for python 2.0
///                   added from .h
  PyBufferProcs = {$IFNDEF CPUX64}packed{$ENDIF} record
     bf_getreadbuffer   : getreadbufferproc;
     bf_getwritebuffer  : getwritebufferproc;
     bf_getsegcount     : getsegcountproc;
     bf_getcharbuffer   : getcharbufferproc;
  end;
  PPyBufferProcs = ^PyBufferProcs;

  Py_complex =  {$IFNDEF CPUX64}packed{$ENDIF} record
     real : double;
     imag : double;
  end;

  PyObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt: NativeInt;
    ob_type:   PPyTypeObject;
  end;

  PyIntObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    ob_ival   : LongInt;
  end;

  _frozen = {$IFNDEF CPUX64}packed{$ENDIF} record
     name	: PAnsiChar;
     code	: PByte;
     size	: Integer;
  end;

  PySliceObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt:          NativeInt;
    ob_type:            PPyTypeObject;
    start, stop, step:  PPyObject;
  end;

  PPyMethodDef = ^PyMethodDef;
  PyMethodDef  = {$IFNDEF CPUX64}packed{$ENDIF} record
     ml_name:  PAnsiChar;
     ml_meth:  PyCFunction;
     ml_flags: Integer;
     ml_doc:   PAnsiChar;
  end;

  // structmember.h
  PPyMemberDef = ^PyMemberDef;
  PyMemberDef = {$IFNDEF CPUX64}packed{$ENDIF} record
    name : PAnsiChar;
    _type : integer;
    offset : NativeInt;
    flags : integer;
    doc : PAnsiChar;
  end;

  // descrobject.h

  // Descriptors

  getter = function ( obj : PPyObject; context : Pointer) : PPyObject; cdecl;
  setter = function ( obj, value : PPyObject; context : Pointer) : integer; cdecl;

  PPyGetSetDef = ^PyGetSetDef;
  PyGetSetDef = {$IFNDEF CPUX64}packed{$ENDIF} record
    name : PAnsiChar;
    get : getter;
    _set : setter;
    doc : PAnsiChar;
    closure : Pointer;
  end;

  wrapperfunc = function (self, args: PPyObject; wrapped : Pointer) : PPyObject; cdecl;

  pwrapperbase = ^wrapperbase;
  wrapperbase = {$IFNDEF CPUX64}packed{$ENDIF} record
    name : PAnsiChar;
    wrapper : wrapperfunc;
    doc : PAnsiChar;
  end;

  // Various kinds of descriptor objects

  {#define PyDescr_COMMON \
          PyObject_HEAD \
          PyTypeObject *d_type; \
          PyObject *d_name
  }

  PPyDescrObject = ^PyDescrObject;
  PyDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
  end;

  PPyMethodDescrObject = ^PyMethodDescrObject;
  PyMethodDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_method : PPyMethodDef;
  end;

  PPyMemberDescrObject = ^PyMemberDescrObject;
  PyMemberDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_member : PPyMemberDef;
  end;

  PPyGetSetDescrObject = ^PyGetSetDescrObject;
  PyGetSetDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_getset : PPyGetSetDef;
  end;

  PPyWrapperDescrObject = ^PyWrapperDescrObject;
  PyWrapperDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_base : pwrapperbase;
    d_wrapped : Pointer; // This can be any function pointer
  end;

  PPyModuleDef_Base = ^PyModuleDef_Base;
  PyModuleDef_Base = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    m_init     : function( ) : PPyObject; cdecl;
    m_index     : NativeInt;
    m_copy : PPyObject;
  end;

  PPyModuleDef = ^PyModuleDef;
  PyModuleDef = {$IFNDEF CPUX64}packed{$ENDIF} record
    m_base : PyModuleDef_Base;
    m_name : PAnsiChar;
    m_doc : PAnsiChar;
    m_size : NativeInt;
    m_methods : PPyMethodDef;
    m_reload : inquiry;
    m_traverse : traverseproc;
    m_clear : inquiry;
    m_free : inquiry;
  end;


  // object.h
  PyTypeObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt:      NativeInt;
    ob_type:        PPyTypeObject;
    ob_size:        NativeInt; // Number of items in variable part
    tp_name:        PAnsiChar;   // For printing
    tp_basicsize, tp_itemsize: NativeInt; // For allocation

    // Methods to implement standard operations

    tp_dealloc:     pydestructor;
    tp_print:       printfunc;
    tp_getattr:     getattrfunc;
    tp_setattr:     setattrfunc;
    tp_compare:     cmpfunc;
    tp_repr:        reprfunc;

    // Method suites for standard classes

    tp_as_number:   PPyNumberMethods;
    tp_as_sequence: PPySequenceMethods;
    tp_as_mapping:  PPyMappingMethods;

    // More standard operations (here for binary compatibility)

    tp_hash:        hashfunc;
    tp_call:        ternaryfunc;
    tp_str:         reprfunc;
    tp_getattro:    getattrofunc;
    tp_setattro:    setattrofunc;

/// jah 29-sep-2000 : updated for python 2.0

    // Functions to access object as input/output buffer
    tp_as_buffer:   PPyBufferProcs;
    // Flags to define presence of optional/expanded features
    tp_flags:       LongInt;

    tp_doc:         PAnsiChar; // Documentation string

    // call function for all accessible objects
    tp_traverse:    traverseproc;

    // delete references to contained objects
    tp_clear:       inquiry;

    // rich comparisons
    tp_richcompare: richcmpfunc;

    // weak reference enabler
    tp_weaklistoffset: NativeInt;
    // Iterators
    tp_iter : getiterfunc;
    tp_iternext : iternextfunc;

    // Attribute descriptor and subclassing stuff
    tp_methods          : PPyMethodDef;
    tp_members          : PPyMemberDef;
    tp_getset           : PPyGetSetDef;
    tp_base             : PPyTypeObject;
    tp_dict             : PPyObject;
    tp_descr_get        : descrgetfunc;
    tp_descr_set        : descrsetfunc;
    tp_dictoffset       : NativeInt;
    tp_init             : initproc;
    tp_alloc            : allocfunc;
    tp_new              : newfunc;
    tp_free             : pydestructor; // Low-level free-memory routine
    tp_is_gc            : inquiry; // For PyObject_IS_GC
    tp_bases            : PPyObject;
    tp_mro              : PPyObject; // method resolution order
    tp_cache            : PPyObject;
    tp_subclasses       : PPyObject;
    tp_weaklist         : PPyObject;
    //More spares
    tp_xxx7             : NativeInt;
    tp_xxx8             : LongInt;
  end;

  PPyMethodChain = ^PyMethodChain;
  PyMethodChain = {$IFNDEF CPUX64}packed{$ENDIF} record
    methods: PPyMethodDef;
    link:    PPyMethodChain;
  end;

  PPyClassObject = ^PyClassObject;
  PyClassObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    cl_bases   : PPyObject;       // A tuple of class objects
    cl_dict    : PPyObject;       // A dictionary
    cl_name    : PPyObject;       // A string
    // The following three are functions or NULL
    cl_getattr : PPyObject;
    cl_setattr : PPyObject;
    cl_delattr : PPyObject;
  end;

  PPyInstanceObject = ^PyInstanceObject;
  PyInstanceObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    in_class  : PPyClassObject;      // The class object
    in_dict   : PPyObject;           // A dictionary
  end;

{ Instance method objects are used for two purposes:
   (a) as bound instance methods (returned by instancename.methodname)
   (b) as unbound methods (returned by ClassName.methodname)
   In case (b), im_self is NULL
}

  PPyMethodObject = ^PyMethodObject;
  PyMethodObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    im_func  : PPyObject;      // The function implementing the method
    im_self  : PPyObject;      // The instance it is bound to, or NULL
    im_class : PPyObject;      // The class that defined the method
  end;


  // Bytecode object, compile.h
  PPyCodeObject = ^PyCodeObject;
  PyCodeObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt      : NativeInt;
    ob_type        : PPyTypeObject;
    co_argcount    : Integer;         // #arguments, except *args
    co_nlocals     : Integer;         // #local variables
    co_stacksize   : Integer;          // #entries needed for evaluation stack
    co_flags       : Integer;         // CO_..., see below
    co_code        : PPyObject;       // instruction opcodes (it hides a PyStringObject)
    co_consts      : PPyObject;       // list (constants used)
    co_names       : PPyObject;       // list of strings (names used)
    co_varnames    : PPyObject;       // tuple of strings (local variable names)
    co_freevars    : PPyObject;	      // tuple of strings (free variable names)
    co_cellvars    : PPyObject;       // tuple of strings (cell variable names)
    // The rest doesn't count for hash/cmp
    co_filename    : PPyObject;       // string (where it was loaded from)
    co_name        : PPyObject;       // string (name, for reference)
    co_firstlineno : Integer;         // first source line number
    co_lnotab      : PPyObject;       // string (encoding addr<->lineno mapping)
  end;


  // from pystate.h
  PPyInterpreterState = ^PyInterpreterState;
  PPyThreadState = ^PyThreadState;
  PPyFrameObject = ^PyFrameObject;

  // Interpreter environments
  PyInterpreterState = {$IFNDEF CPUX64}packed{$ENDIF} record
    next           : PPyInterpreterState;
    tstate_head    : PPyThreadState;

    modules        : PPyObject;
    sysdict        : PPyObject;
    builtins       : PPyObject;

    checkinterval  : integer;
  end;

  // Thread specific information
  PyThreadState = {$IFNDEF CPUX64}packed{$ENDIF} record
    next           : PPyThreadState;
    interp         : PPyInterpreterState;

    frame          : PPyFrameObject;
    recursion_depth: integer;
    ticker         : integer;
    tracing        : integer;

    sys_profilefn  : Pointer;           // c-functions for profile/trace
    sys_tracefn    : Pointer;
    sys_profilefunc: PPyObject;
    sys_tracefunc  : PPyObject;

    curexc_type    : PPyObject;
    curexc_value   : PPyObject;
    curexc_traceback: PPyObject;

    exc_type       : PPyObject;
    exc_value      : PPyObject;
    exc_traceback  : PPyObject;

    dict           : PPyObject;
    tick_counter      :Integer;
    gilstate_counter  :Integer;

    async_exc         :PPyObject; { Asynchronous exception to raise }
    thread_id         :LongInt;   { Thread id where this tstate was created }

    { XXX signal handlers should also be here }
  end;

  // from frameobject.h

  PPyTryBlock = ^PyTryBlock;
  PyTryBlock = {$IFNDEF CPUX64}packed{$ENDIF} record
    b_type    : Integer;       // what kind of block this is
    b_handler : Integer;       // where to jump to find handler
    b_level   : Integer;       // value stack level to pop to
  end;

  CO_MAXBLOCKS  = 0..19;
  PyFrameObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the VAR_HEAD of an object.
    ob_refcnt    : NativeInt;
    ob_type      : PPyTypeObject;
    ob_size      : NativeInt;           // Number of items in variable part
    // End of the Head of an object
    f_back       : PPyFrameObject;    // previous frame, or NULL
    f_code       : PPyCodeObject;     // code segment
    f_builtins   : PPyObject;         // builtin symbol table (PyDictObject)
    f_globals    : PPyObject;         // global symbol table (PyDictObject)
    f_locals     : PPyObject;         // local symbol table (PyDictObject)
    f_valuestack : PPPyObject;        // points after the last local
    (* Next free slot in f_valuestack.  Frame creation sets to f_valuestack.
       Frame evaluation usually NULLs it, but a frame that yields sets it
       to the current stack top. *)
    f_stacktop   : PPPyObject;
    f_trace      : PPyObject;         // Trace function
    f_exc_type, f_exc_value, f_exc_traceback: PPyObject;
    f_tstate     : PPyThreadState;
    f_lasti      : Integer;           // Last instruction if called
    f_lineno     : Integer;           // Current line number
    f_iblock     : Integer;           // index in f_blockstack
    f_blockstack : array[CO_MAXBLOCKS] of PyTryBlock; // for try and loop blocks
    f_localsplus : array[0..0] of PPyObject; // locals+stack, dynamically sized
  end;

  // From traceback.c
  PPyTraceBackObject = ^PyTraceBackObject;
  PyTraceBackObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    tb_next   : PPyTraceBackObject;
    tb_frame  : PPyFrameObject;
    tb_lasti  : Integer;
    tb_lineno : Integer;
  end;

  // Parse tree node interface

  PNode = ^node;
  node = {$IFNDEF CPUX64}packed{$ENDIF} record
    n_type      : smallint;
    n_str       : PAnsiChar;
    n_lineno    : integer;
    n_col_offset: integer;
    n_nchildren : integer;
    n_child     : PNode;
  end;

  // From weakrefobject.h

  PPyWeakReference = ^PyWeakReference;
  PyWeakReference = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    wr_object   : PPyObject;
    wr_callback : PPyObject;
    hash        : NativeInt;
    wr_prev     : PPyWeakReference;
    wr_next     : PPyWeakReference;
  end;

  // from datetime.h


{* Fields are packed into successive bytes, each viewed as unsigned and
 * big-endian, unless otherwise noted:
 *
 * byte offset
 *  0 		year     2 bytes, 1-9999
 *  2	  	month    1 byte,  1-12
 *  3 		day      1 byte,  1-31
 *  4     hour     1 byte,  0-23
 *  5 		minute   1 byte,  0-59
 *  6 		second   1 byte,  0-59
 *  7 		usecond  3 bytes, 0-999999
 * 10
 *}

const
  { # of bytes for year, month, and day. }
  _PyDateTime_DATE_DATASIZE = 4;

  { # of bytes for hour, minute, second, and usecond. }
  _PyDateTime_TIME_DATASIZE = 6;

  { # of bytes for year, month, day, hour, minute, second, and usecond. }
  _PyDateTime_DATETIME_DATASIZE = 10;
type
  PyDateTime_Delta = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode    : Integer;  // -1 when unknown
    days        : Integer;  // -MAX_DELTA_DAYS <= days <= MAX_DELTA_DAYS
    seconds     : Integer;  // 0 <= seconds < 24*3600 is invariant
    microseconds: Integer;  // 0 <= microseconds < 1000000 is invariant
  end;
  PPyDateTime_Delta = ^PyDateTime_Delta;

  PyDateTime_TZInfo = {$IFNDEF CPUX64}packed{$ENDIF} record // a pure abstract base clase
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
  end;
  PPyDateTime_TZInfo = ^PyDateTime_TZInfo;

{
/* The datetime and time types have hashcodes, and an optional tzinfo member,
 * present if and only if hastzinfo is true.
 */
#define _PyTZINFO_HEAD		\
	PyObject_HEAD		\
	long hashcode;		\
	char hastzinfo;		/* boolean flag */
}

{* No _PyDateTime_BaseTZInfo is allocated; it's just to have something
 * convenient to cast to, when getting at the hastzinfo member of objects
 * starting with _PyTZINFO_HEAD.
 *}
  _PyDateTime_BaseTZInfo = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
  end;
  _PPyDateTime_BaseTZInfo = ^_PyDateTime_BaseTZInfo;

{* All time objects are of PyDateTime_TimeType, but that can be allocated
 * in two ways, with or without a tzinfo member.  Without is the same as
 * tzinfo == None, but consumes less memory.  _PyDateTime_BaseTime is an
 * internal struct used to allocate the right amount of space for the
 * "without" case.
 *}
{#define _PyDateTime_TIMEHEAD	\
	_PyTZINFO_HEAD		\
	unsigned char data[_PyDateTime_TIME_DATASIZE];
}

  _PyDateTime_BaseTime = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo false
    // Start of _PyDateTime_TIMEHEAD
      // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_TIME_DATASIZE)] of Byte;
    // End of _PyDateTime_TIMEHEAD
  end;
  _PPyDateTime_BaseTime = ^_PyDateTime_BaseTime;

  PyDateTime_Time = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo true
    // Start of _PyDateTime_TIMEHEAD
      // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_TIME_DATASIZE)] of Byte;
    // End of _PyDateTime_TIMEHEAD
    tzinfo     : PPyObject;
  end;
  PPyDateTime_Time = ^PyDateTime_Time;



{* All datetime objects are of PyDateTime_DateTimeType, but that can be
 * allocated in two ways too, just like for time objects above.  In addition,
 * the plain date type is a base class for datetime, so it must also have
 * a hastzinfo member (although it's unused there).
 *}
  PyDateTime_Date = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_DATE_DATASIZE)] of Byte;
  end;
  PPyDateTime_Date = ^PyDateTime_Date;

 {
#define _PyDateTime_DATETIMEHEAD	\
	_PyTZINFO_HEAD			\
	unsigned char data[_PyDateTime_DATETIME_DATASIZE];
}

  _PyDateTime_BaseDateTime = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo false
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_DATETIME_DATASIZE)] of Byte;
  end;
  _PPyDateTime_BaseDateTime = ^_PyDateTime_BaseDateTime;

  PyDateTime_DateTime = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo true
    // Start of _PyDateTime_DATETIMEHEAD
      // Start of _PyTZINFO_HEAD
        // Start of the Head of an object
        ob_refcnt  : NativeInt;
        ob_type    : PPyTypeObject;
        // End of the Head of an object
      hashcode   : Integer;
      hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
      data       : array[0..Pred(_PyDateTime_DATETIME_DATASIZE)] of Byte;
    // End of _PyDateTime_DATETIMEHEAD
    tzinfo : PPyObject;
  end;
  PPyDateTime_DateTime = ^PyDateTime_DateTime;

//#######################################################
//##                                                   ##
//##         GIL state                                 ##
//##                                                   ##
//#######################################################
  PyGILState_STATE = (PyGILState_LOCKED, PyGILState_UNLOCKED);

//#######################################################
//##                                                   ##
//##         New exception classes                     ##
//##                                                   ##
//#######################################################

  // Components' exceptions
  EDLLLoadError  = class(Exception);
  EDLLImportError = class(Exception)
    public
      WrongFunc : AnsiString;
      ErrorCode : Integer;
  end;

  // Python's exceptions
  EPythonError   = class(Exception)
    public
      EName : String;
      EValue : String;
  end;
  EPyExecError   = class(EPythonError);


  // Standard exception classes of Python

/// jah 29-sep-2000 : updated for python 2.0
///                   base classes updated according python documentation

{ Hierarchy of Python exceptions, Python 2.3, copied from <INSTALL>\Python\exceptions.c

Exception\n\
 |\n\
 +-- SystemExit\n\
 +-- StopIteration\n\
 +-- StandardError\n\
 |    |\n\
 |    +-- KeyboardInterrupt\n\
 |    +-- ImportError\n\
 |    +-- EnvironmentError\n\
 |    |    |\n\
 |    |    +-- IOError\n\
 |    |    +-- OSError\n\
 |    |         |\n\
 |    |         +-- WindowsError\n\
 |    |         +-- VMSError\n\
 |    |\n\
 |    +-- EOFError\n\
 |    +-- RuntimeError\n\
 |    |    |\n\
 |    |    +-- NotImplementedError\n\
 |    |\n\
 |    +-- NameError\n\
 |    |    |\n\
 |    |    +-- UnboundLocalError\n\
 |    |\n\
 |    +-- AttributeError\n\
 |    +-- SyntaxError\n\
 |    |    |\n\
 |    |    +-- IndentationError\n\
 |    |         |\n\
 |    |         +-- TabError\n\
 |    |\n\
 |    +-- TypeError\n\
 |    +-- AssertionError\n\
 |    +-- LookupError\n\
 |    |    |\n\
 |    |    +-- IndexError\n\
 |    |    +-- KeyError\n\
 |    |\n\
 |    +-- ArithmeticError\n\
 |    |    |\n\
 |    |    +-- OverflowError\n\
 |    |    +-- ZeroDivisionError\n\
 |    |    +-- FloatingPointError\n\
 |    |\n\
 |    +-- ValueError\n\
 |    |    |\n\
 |    |    +-- UnicodeError\n\
 |    |        |\n\
 |    |        +-- UnicodeEncodeError\n\
 |    |        +-- UnicodeDecodeError\n\
 |    |        +-- UnicodeTranslateError\n\
 |    |\n\
 |    +-- ReferenceError\n\
 |    +-- SystemError\n\
 |    +-- MemoryError\n\
 |\n\
 +---Warning\n\
      |\n\
      +-- UserWarning\n\
      +-- DeprecationWarning\n\
      +-- PendingDeprecationWarning\n\
      +-- SyntaxWarning\n\
      +-- RuntimeWarning\n\
      +-- FutureWarning"
}
   EPyException = class (EPythonError);
   EPyStandardError = class (EPyException);
   EPyArithmeticError = class (EPyStandardError);
   EPyLookupError = class (EPyStandardError);
   EPyAssertionError = class (EPyStandardError);
   EPyAttributeError = class (EPyStandardError);
   EPyEOFError = class (EPyStandardError);
   EPyFloatingPointError = class (EPyArithmeticError);
   EPyEnvironmentError = class (EPyStandardError);
   EPyIOError = class (EPyEnvironmentError);
   EPyOSError = class (EPyEnvironmentError);
   EPyImportError = class (EPyStandardError);
   EPyIndexError = class (EPyLookupError);
   EPyKeyError = class (EPyLookupError);
   EPyKeyboardInterrupt = class (EPyStandardError);
   EPyMemoryError = class (EPyStandardError);
   EPyNameError = class (EPyStandardError);
   EPyOverflowError = class (EPyArithmeticError);
   EPyRuntimeError = class (EPyStandardError);
   EPyNotImplementedError = class (EPyRuntimeError);
   EPySyntaxError = class (EPyStandardError)
   public
      EFileName: string;
      ELineStr: string;
      ELineNumber: Integer;
      EOffset: Integer;
   end;
   EPyIndentationError = class (EPySyntaxError);
   EPyTabError = class (EPyIndentationError);
   EPySystemError = class (EPyStandardError);
   EPySystemExit = class (EPyException);
   EPyTypeError = class (EPyStandardError);
   EPyUnboundLocalError = class (EPyNameError);
   EPyValueError = class (EPyStandardError);
   EPyUnicodeError = class (EPyValueError);
   UnicodeEncodeError = class (EPyUnicodeError);
   UnicodeDecodeError = class (EPyUnicodeError);
   UnicodeTranslateError = class (EPyUnicodeError);
   EPyZeroDivisionError = class (EPyArithmeticError);
   EPyStopIteration = class(EPyException);
   EPyWarning = class (EPyException);
   EPyUserWarning = class (EPyWarning);
   EPyDeprecationWarning = class (EPyWarning);
   PendingDeprecationWarning = class (EPyWarning);
   FutureWarning = class (EPyWarning);
   EPySyntaxWarning = class (EPyWarning);
   EPyRuntimeWarning = class (EPyWarning);
   EPyReferenceError = class (EPyStandardError);
 {$IFDEF MSWINDOWS}
   EPyWindowsError = class (EPyOSError);
 {$ENDIF}

//#######################################################
//##                                                   ##
//##                   Components                      ##
//##                                                   ##
//#######################################################

//-------------------------------------------------------
//--                                                   --
//--      class:  TPythonInputOutput                   --
//--      Works as a console for Python outputs        --
//--      It's a virtual Base class                    --
//-------------------------------------------------------

const
  kMaxLines = 1000;
  kMaxLineLength = 256;

type
  TSendDataEvent = procedure (Sender: TObject; const Data : AnsiString ) of object;
  TReceiveDataEvent = procedure (Sender: TObject; var Data : AnsiString ) of object;
  TSendUniDataEvent = procedure (Sender: TObject; const Data : UnicodeString ) of object;
  TReceiveUniDataEvent = procedure (Sender: TObject; var Data : UnicodeString ) of object;
  IOChar = WideChar;
  IOString = UnicodeString;
  TIOStringList = TUnicodeStringList;

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonInputOutput = class(TComponent)
  protected
    FMaxLines        : Integer;
    FLine_Buffer     : IOString;
    FLinesPerThread  : TIOStringList;
    FLock            : TCriticalSection;
    FQueue           : TIOStringList;
    FDelayWrites     : Boolean;
    FMaxLineLength   : Integer;
    FOnSendData      : TSendDataEvent;
    FOnReceiveData   : TReceiveDataEvent;
    FOnSendUniData   : TSendUniDataEvent;
    FOnReceiveUniData: TReceiveUniDataEvent;
    FUnicodeIO       : Boolean;
    FRawOutput       : Boolean;

    procedure Lock;
    procedure Unlock;
    procedure AddWrite( const str : IOString );
    // Virtual methods for handling the input/output of text
    procedure SendData( const Data : AnsiString ); virtual;
    function  ReceiveData : AnsiString; virtual;
    procedure SendUniData( const Data : UnicodeString ); virtual;
    function  ReceiveUniData : UnicodeString; virtual;
    procedure AddPendingWrite; virtual;
    function  GetCurrentThreadSlotIdx : Integer;
    function  GetCurrentThreadLine : IOString;
    procedure UpdateCurrentThreadLine;

  public
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure Write( const str : IOString );
    procedure WriteLine( const str : IOString );

  published
    property MaxLines : Integer read FMaxLines write FMaxLines default kMaxLines;
    property MaxLineLength : Integer read FMaxLineLength write FMaxLineLength default kMaxLineLength;
    property DelayWrites : Boolean read FDelayWrites write FDelayWrites default False;
    property OnSendData    : TSendDataEvent read FOnSendData write FOnSendData;
    property OnReceiveData : TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    property OnSendUniData    : TSendUniDataEvent read FOnSendUniData write FOnSendUniData;
    property OnReceiveUniData : TReceiveUniDataEvent read FOnReceiveUniData write FOnReceiveUniData;
    property UnicodeIO: Boolean read FUnicodeIO write FUnicodeIO;
    property RawOutput: Boolean read FRawOutput write FRawOutput;
  end;

//-------------------------------------------------------
//--                                                   --
//--      Base class:  TDynamicDll                     --
//--                                                   --
//-------------------------------------------------------

type
  TDynamicDll = class(TComponent)
  private
    function IsAPIVersionStored: Boolean;
    function IsDllNameStored: Boolean;
    function IsRegVersionStored: Boolean;
    procedure SetDllName(const Value: String);
  protected
    FDllName            : String;
    FDllPath            : String;
    FAPIVersion         : Integer;
    FRegVersion         : String;
    FAutoLoad           : Boolean;
    FAutoUnload         : Boolean;
    FFatalMsgDlg        : Boolean;
    FFatalAbort         : Boolean;
    FDLLHandle          : THandle;
    FUseLastKnownVersion: Boolean;
    FOnBeforeLoad       : TNotifyEvent;
    FOnAfterLoad        : TNotifyEvent;
    FOnBeforeUnload     : TNotifyEvent;

    function  Import(const funcname: AnsiString; canFail : Boolean = True): Pointer;
    procedure Loaded; override;
    procedure BeforeLoad; virtual;
    procedure AfterLoad; virtual;
    procedure BeforeUnload; virtual;
    function  GetQuitMessage : String; virtual;
    procedure DoOpenDll(const aDllName : String); virtual;
    function  GetDllPath : String;

  public
    // Constructors & Destructors
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;                    override;

    // Public methods
    procedure OpenDll(const aDllName : String);
    function  IsHandleValid : Boolean;
    procedure LoadDll;
    procedure UnloadDll;
    procedure Quit;

    // Public properties
  published
    property AutoLoad : Boolean read FAutoLoad write FAutoLoad default True;
    property AutoUnload : Boolean read FAutoUnload write FAutoUnload default True;
    property DllName : String read FDllName write SetDllName stored IsDllNameStored;
    property DllPath : String read FDllPath write FDllPath;
    property APIVersion : Integer read FAPIVersion write FAPIVersion stored IsAPIVersionStored;
    property RegVersion : String read FRegVersion write FRegVersion stored IsRegVersionStored;
    property FatalAbort :  Boolean read FFatalAbort write FFatalAbort default True;
    property FatalMsgDlg : Boolean read FFatalMsgDlg write FFatalMsgDlg default True;
    property UseLastKnownVersion: Boolean read FUseLastKnownVersion write FUseLastKnownVersion default True;
    property OnAfterLoad : TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
    property OnBeforeLoad : TNotifyEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnBeforeUnload : TNotifyEvent read FOnBeforeUnload write FOnBeforeUnload;
  end;

//-------------------------------------------------------
//--                                                   --
//--  class:  TPythonInterface derived from TDynamicDll--
//--      This class maps the functions imported       --
//--      from the Python Dll, and adds some           --
//--      Delphi implementations.                      --
//-------------------------------------------------------

type
  TPythonInterface=class(TDynamicDll)
  private
    DLL_Py_GetBuildInfo:
                     function : PAnsiChar; cdecl;
    DLL_PyCode_Addr2Line:
                     function ( co: PPyCodeObject; addrq : Integer ) : Integer; cdecl;
    DLL_PyImport_ExecCodeModule:
                     function ( const name : AnsiString; codeobject : PPyObject) : PPyObject; cdecl;

    DLL_PyString_FromString:  function( str: PAnsiChar): PPyObject; cdecl;
    DLL_Py_FlushLine:procedure; cdecl;

  protected
    FInitialized:    Boolean;
    FFinalizing:     Boolean;
    FIsPython3000:   Boolean;
    FMajorVersion:   integer;
    FMinorVersion:   integer;
    FBuiltInModuleName: String;
    function GetInitialized: Boolean;

    procedure AfterLoad; override;
    function  GetQuitMessage : String; override;
    procedure CheckPython;
    function  GetUnicodeTypeSuffix : String;

  public
    // define Python flags. See file pyDebug.h
    Py_DebugFlag: PInt;
    Py_VerboseFlag: PInt;
    Py_InteractiveFlag: PInt;
    Py_OptimizeFlag: PInt;
    Py_NoSiteFlag: PInt;
    Py_UseClassExceptionsFlag: PInt;
    Py_FrozenFlag: PInt;
    Py_TabcheckFlag: PInt;
    Py_UnicodeFlag: PInt;
    Py_IgnoreEnvironmentFlag: PInt;
    Py_DivisionWarningFlag: PInt;
    //_PySys_TraceFunc:    PPPyObject;
    //_PySys_ProfileFunc: PPPPyObject;

    PyImport_FrozenModules: PP_frozen;

    Py_None:            PPyObject;
    Py_Ellipsis:        PPyObject;
    Py_False:           PPyIntObject;
    Py_True:            PPyIntObject;
    Py_NotImplemented:  PPyObject;

    PyExc_AttributeError: PPPyObject;
    PyExc_EOFError: PPPyObject;
    PyExc_IOError: PPPyObject;
    PyExc_ImportError: PPPyObject;
    PyExc_IndexError: PPPyObject;
    PyExc_KeyError: PPPyObject;
    PyExc_KeyboardInterrupt: PPPyObject;
    PyExc_MemoryError: PPPyObject;
    PyExc_NameError: PPPyObject;
    PyExc_OverflowError: PPPyObject;
    PyExc_RuntimeError: PPPyObject;
    PyExc_SyntaxError: PPPyObject;
    PyExc_SystemError: PPPyObject;
    PyExc_SystemExit: PPPyObject;
    PyExc_TypeError: PPPyObject;
    PyExc_ValueError: PPPyObject;
    PyExc_ZeroDivisionError: PPPyObject;
    PyExc_ArithmeticError: PPPyObject;
    PyExc_Exception: PPPyObject;
    PyExc_FloatingPointError: PPPyObject;
    PyExc_LookupError: PPPyObject;
    PyExc_StandardError: PPPyObject;
    PyExc_AssertionError: PPPyObject;
    PyExc_EnvironmentError: PPPyObject;
    PyExc_IndentationError: PPPyObject;
    PyExc_MemoryErrorInst: PPPyObject;
    PyExc_NotImplementedError: PPPyObject;
    PyExc_OSError: PPPyObject;
    PyExc_TabError: PPPyObject;
    PyExc_UnboundLocalError: PPPyObject;
    PyExc_UnicodeError: PPPyObject;
 {$IFDEF MSWINDOWS}
    PyExc_WindowsError: PPPyObject;
 {$ENDIF}
    PyExc_Warning: PPPyObject;
    PyExc_DeprecationWarning: PPPyObject;
    PyExc_RuntimeWarning: PPPyObject;
    PyExc_SyntaxWarning: PPPyObject;
    PyExc_UserWarning: PPPyObject;
    PyExc_ReferenceError: PPPyObject;
    PyExc_StopIteration: PPPyObject;
    PyExc_FutureWarning: PPPyObject;
    PyExc_PendingDeprecationWarning: PPPyObject;
    PyExc_UnicodeDecodeError: PPPyObject;
    PyExc_UnicodeEncodeError: PPPyObject;
    PyExc_UnicodeTranslateError: PPPyObject;

    PyType_Type: PPyTypeObject;
    PyCFunction_Type: PPyTypeObject;
    PyCObject_Type: PPyTypeObject;
    PyClass_Type: PPyTypeObject;
    PyCode_Type: PPyTypeObject;
    PyComplex_Type: PPyTypeObject;
    PyDict_Type: PPyTypeObject;
    PyFile_Type: PPyTypeObject;
    PyFloat_Type: PPyTypeObject;
    PyFrame_Type: PPyTypeObject;
    PyFunction_Type: PPyTypeObject;
    PyInstance_Type: PPyTypeObject;
    PyInt_Type: PPyTypeObject;
    PyList_Type: PPyTypeObject;
    PyLong_Type: PPyTypeObject;
    PyMethod_Type: PPyTypeObject;
    PyModule_Type: PPyTypeObject;
    PyObject_Type: PPyTypeObject;
    PyRange_Type: PPyTypeObject;
    PySlice_Type: PPyTypeObject;
    PyString_Type: PPyTypeObject;
    PyTuple_Type: PPyTypeObject;
    PyBaseObject_Type: PPyTypeObject;
    PyBuffer_Type: PPyTypeObject;
    PyCallIter_Type: PPyTypeObject;
    PyCell_Type: PPyTypeObject;
    PyClassMethod_Type: PPyTypeObject;
    PyProperty_Type: PPyTypeObject;
    PySeqIter_Type: PPyTypeObject;
    PyStaticMethod_Type: PPyTypeObject;
    PySuper_Type: PPyTypeObject;
    PyTraceBack_Type: PPyTypeObject;
    PyUnicode_Type: PPyTypeObject;
    PyWrapperDescr_Type: PPyTypeObject;
    _PyWeakref_RefType: PPyTypeObject;
    _PyWeakref_ProxyType: PPyTypeObject;
    _PyWeakref_CallableProxyType: PPyTypeObject;
    PyBaseString_Type: PPyTypeObject;
    PyBool_Type: PPyTypeObject;
    PyEnum_Type: PPyTypeObject;

    //PyArg_GetObject: function(args : PPyObject; nargs, i: integer; p_a: PPPyObject): integer; cdecl;
    //PyArg_GetLong:   function(args : PPyObject; nargs, i: integer; p_a: PLong): integer; cdecl;
    //PyArg_GetShort:  function(args : PPyObject; nargs, i: integer; p_a: PShort): integer; cdecl;
    //PyArg_GetFloat:  function(args : PPyObject; nargs, i: integer; p_a: PFloat): integer; cdecl;
    //PyArg_GetString: function(args : PPyObject; nargs, i: integer; p_a: PAnsiString): integer; cdecl;
    //PyArgs_VaParse:  function (args : PPyObject; format: PAnsiChar; va_list: array of const): integer; cdecl;
    // Does not work!
    // Py_VaBuildValue: function (format: PAnsiChar; va_list: array of const): PPyObject; cdecl;
    //PyBuiltin_Init:     procedure; cdecl;

    PyComplex_FromCComplex: function(c: Py_complex):PPyObject; cdecl;
    PyComplex_FromDoubles: function(realv,imag : double):PPyObject; cdecl;
    PyComplex_RealAsDouble: function(op : PPyObject ): double; cdecl;
    PyComplex_ImagAsDouble: function(op : PPyObject ): double; cdecl;
    PyComplex_AsCComplex: function(op : PPyObject ): Py_complex; cdecl;
    PyCFunction_GetFunction: function(ob : PPyObject): Pointer; cdecl;
    PyCFunction_GetSelf: function(ob : PPyObject): PPyObject; cdecl;
    PyCallable_Check: function(ob	: PPyObject): integer; cdecl;
    PyCObject_FromVoidPtr: function(cobj, destruct : Pointer): PPyObject; cdecl;
    PyCObject_AsVoidPtr: function(ob : PPyObject): Pointer; cdecl;
    PyClass_New: function (ob1,ob2,ob3 :  PPyObject): PPyObject; cdecl;
    PyClass_IsSubclass: function (ob1, ob2 : PPyObject): integer cdecl;

    Py_InitModule4: function( name: PAnsiChar; methods: PPyMethodDef; doc: PAnsiChar;
                              passthrough: PPyObject; Api_Version: Integer):PPyObject; cdecl;
    PyModule_Create2:   function(moduledef: PPyModuleDef; Api_Version: Integer):PPyObject; cdecl;
    PyErr_BadArgument:  function: integer; cdecl;
    PyErr_BadInternalCall: procedure; cdecl;
    PyErr_CheckSignals: function: integer; cdecl;
    PyErr_Clear:        procedure; cdecl;
    PyErr_Fetch:        procedure( errtype, errvalue, errtraceback: PPPyObject); cdecl;
    PyErr_NoMemory:     function: PPyObject; cdecl;
    PyErr_Occurred:     function: PPyObject; cdecl;
    PyErr_Print:        procedure; cdecl;
    PyErr_Restore:      procedure  (errtype, errvalue, errtraceback: PPyObject); cdecl;
    PyErr_SetFromErrno: function (ob :  PPyObject):PPyObject; cdecl;
    PyErr_SetNone:      procedure(value: PPyObject); cdecl;
    PyErr_SetObject:    procedure  (ob1, ob2	: PPyObject); cdecl;
    PyErr_SetString:    procedure( ErrorObject: PPyObject; text: PAnsiChar); cdecl;
    PyImport_GetModuleDict: function: PPyObject; cdecl;
    PyInt_FromLong:     function( x: LongInt):PPyObject; cdecl;
    PyArg_Parse:        function( args: PPyObject; format: PAnsiChar {;....}) :  Integer; cdecl varargs;
    PyArg_ParseTuple:   function( args: PPyObject; format: PAnsiChar {;...}): Integer; cdecl varargs;
    Py_BuildValue:      function( format: PAnsiChar {;...}): PPyObject; cdecl varargs;
    Py_Initialize:      procedure; cdecl;
    Py_Exit:            procedure( RetVal: Integer); cdecl;
    PyEval_GetBuiltins: function: PPyObject; cdecl;
    PyDict_Copy:        function(mp: PPyObject):PPyObject; cdecl;
    PyDict_GetItem:     function(mp, key : PPyObject):PPyObject; cdecl;
    PyDict_SetItem:     function(mp, key, item :PPyObject ):integer; cdecl;
    PyDict_DelItem:     function(mp, key : PPyObject ):integer; cdecl;
    PyDict_Clear:       procedure(mp : PPyObject); cdecl;
    PyDict_Next:        function(mp : PPyObject; pos: PNativeInt; key, value: PPPyObject):integer; cdecl;
    PyDict_Keys:        function(mp: PPyObject):PPyObject; cdecl;
    PyDict_Values:      function(mp: PPyObject):PPyObject; cdecl;
    PyDict_Items:       function(mp: PPyObject):PPyObject; cdecl;
    PyDict_Size:        function(mp: PPyObject):NativeInt; cdecl;
    PyDict_DelItemString: function(dp : PPyObject;key : PAnsiChar ):integer; cdecl;
    PyDict_New: function: PPyObject; cdecl;
    PyDict_GetItemString: function( dp: PPyObject; key: PAnsiChar): PPyObject; cdecl;
    PyDict_SetItemString: function( dp: PPyObject; key: PAnsiChar; item: PPyObject):
                          Integer; cdecl;
    PyDictProxy_New: function (obj : PPyObject) : PPyObject; cdecl;
    PyModule_GetDict:     function( module:PPyObject): PPyObject; cdecl;
    PyObject_Str:         function( v: PPyObject): PPyObject; cdecl;
    PyRun_String:         function( str: PAnsiChar; start: Integer; globals: PPyObject;
                                    locals: PPyObject): PPyObject; cdecl;
    PyRun_SimpleString:   function( str: PAnsiChar): Integer; cdecl;
    PyString_AsString:    function( ob: PPyObject): PAnsiChar; cdecl;
    PySys_SetArgv:        procedure( argc: Integer; argv: PPAnsiChar); cdecl;
    PySys_SetArgv3000:    procedure( argc: Integer; argv: PPWideChar); cdecl;

    PyCFunction_New: function(md:PPyMethodDef;ob:PPyObject):PPyObject; cdecl;
    PyCFunction_NewEx: function(md:PPyMethodDef;self, ob:PPyObject):PPyObject; cdecl;
// Removed.  Use PyEval_CallObjectWithKeywords with third argument nil
//    PyEval_CallObject: function(callable_obj, args:PPyObject):PPyObject; cdecl;
    PyEval_CallObjectWithKeywords:function (callable_obj, args, kw:PPyObject):PPyObject; cdecl;
    PyEval_GetFrame:function :PPyObject; cdecl;
    PyEval_GetGlobals:function :PPyObject; cdecl;
    PyEval_GetLocals:function :PPyObject; cdecl;
    //PyEval_GetOwner:function :PPyObject; cdecl;
    PyEval_GetRestricted:function :integer; cdecl;

    PyEval_InitThreads:procedure; cdecl;
    PyEval_RestoreThread:procedure( tstate: PPyThreadState); cdecl;
    PyEval_SaveThread:function :PPyThreadState; cdecl;

    PyFile_FromString:function (pc1,pc2:PAnsiChar):PPyObject; cdecl;
    PyFile_GetLine:function (ob:PPyObject;i:integer):PPyObject; cdecl;
    PyFile_Name:function (ob:PPyObject):PPyObject; cdecl;
    PyFile_SetBufSize:procedure(ob:PPyObject;i:integer); cdecl;
    PyFile_SoftSpace:function (ob:PPyObject;i:integer):integer; cdecl;
    PyFile_WriteObject:function (ob1,ob2:PPyObject;i:integer):integer; cdecl;
    PyFile_WriteString:procedure(s:PAnsiChar;ob:PPyObject); cdecl;
    PyFloat_AsDouble:function (ob:PPyObject):DOUBLE; cdecl;
    PyFloat_FromDouble:function (db:double):PPyObject; cdecl;
    PyFunction_GetCode:function (ob:PPyObject):PPyObject; cdecl;
    PyFunction_GetGlobals:function (ob:PPyObject):PPyObject; cdecl;
    PyFunction_New:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyImport_AddModule:function (name:PAnsiChar):PPyObject; cdecl;
    PyImport_Cleanup:procedure; cdecl;
    PyImport_GetMagicNumber:function :LONGINT; cdecl;
    PyImport_ImportFrozenModule:function (key:PAnsiChar):integer; cdecl;
    PyImport_ImportModule:function (name:PAnsiChar):PPyObject; cdecl;
    PyImport_Import:function (name:PPyObject):PPyObject; cdecl;
    //PyImport_Init:procedure; cdecl;
    PyImport_ReloadModule:function (ob:PPyObject):PPyObject; cdecl;
    PyInstance_New:function (obClass, obArg, obKW:PPyObject):PPyObject; cdecl;
    PyInt_AsLong:function (ob:PPyObject):LONGINT; cdecl;
    PyList_Append:function (ob1,ob2:PPyObject):integer; cdecl;
    PyList_AsTuple:function (ob:PPyObject):PPyObject; cdecl;
    PyList_GetItem:function (ob:PPyObject;i:NativeInt):PPyObject; cdecl;
    PyList_GetSlice:function (ob:PPyObject;i1,i2:NativeInt):PPyObject; cdecl;
    PyList_Insert:function (dp:PPyObject;idx:NativeInt;item:PPyObject):integer; cdecl;
    PyList_New:function (size:NativeInt):PPyObject; cdecl;
    PyList_Reverse:function (ob:PPyObject):integer; cdecl;
    PyList_SetItem:function (dp:PPyObject;idx:NativeInt;item:PPyObject):integer; cdecl;
    PyList_SetSlice:function (ob:PPyObject;i1,i2:NativeInt;ob2:PPyObject):integer; cdecl;
    PyList_Size:function (ob:PPyObject):NativeInt; cdecl;
    PyList_Sort:function (ob:PPyObject):integer; cdecl;
    PyLong_AsDouble:function (ob:PPyObject):DOUBLE; cdecl;
    PyLong_AsLong:function (ob:PPyObject):LONGINT; cdecl;
    PyLong_FromDouble:function (db:double):PPyObject; cdecl;
    PyLong_FromLong:function (l:longint):PPyObject; cdecl;
    PyLong_FromString:function (pc:PAnsiChar;var ppc:PAnsiChar;i:integer):PPyObject; cdecl;
    PyLong_FromUnsignedLong:function(val:cardinal) : PPyObject; cdecl;
    PyLong_AsUnsignedLong:function(ob:PPyObject) : Cardinal; cdecl;
    PyLong_FromUnicode:function(ob:PPyObject; a, b : integer) : PPyObject; cdecl;
    PyLong_FromLongLong:function(val:Int64) : PPyObject; cdecl;
    PyLong_AsLongLong:function(ob:PPyObject) : Int64; cdecl;
    PyMapping_Check:function (ob:PPyObject):integer; cdecl;
    PyMapping_GetItemString:function (ob:PPyObject;key:PAnsiChar):PPyObject; cdecl;
    PyMapping_HasKey:function (ob,key:PPyObject):integer; cdecl;
    PyMapping_HasKeyString:function (ob:PPyObject;key:PAnsiChar):integer; cdecl;
    PyMapping_Length:function (ob:PPyObject):NativeInt; cdecl;
    PyMapping_SetItemString:function (ob:PPyObject; key:PAnsiChar; value:PPyObject):integer; cdecl;
    PyMethod_Class:function (ob:PPyObject):PPyObject; cdecl;
    PyMethod_Function:function (ob:PPyObject):PPyObject; cdecl;
    PyMethod_New:function (ob1,ob2,ob3:PPyObject):PPyObject; cdecl;
    PyMethod_Self:function (ob:PPyObject):PPyObject; cdecl;
    PyModule_GetName:function (ob:PPyObject):PAnsiChar; cdecl;
    PyModule_New:function (key:PAnsiChar):PPyObject; cdecl;
    PyNumber_Absolute:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Add:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_And:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Check:function (ob:PPyObject):integer; cdecl;
    PyNumber_Coerce:function (var ob1,ob2:PPyObject):integer; cdecl;
    PyNumber_Divide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_FloorDivide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_TrueDivide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Divmod:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Float:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Int:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Invert:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Long:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Lshift:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Multiply:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Negative:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Or:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Positive:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Power:function (ob1,ob2,ob3:PPyObject):PPyObject; cdecl;
    PyNumber_Remainder:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Rshift:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Subtract:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Xor:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyOS_InitInterrupts:procedure; cdecl;
    PyOS_InterruptOccurred:function :integer; cdecl;
    PyObject_CallObject:function (ob,args:PPyObject):PPyObject; cdecl;
    PyObject_CallMethod : function ( obj : PPyObject; method, format : PAnsiChar {...}) : PPyObject; cdecl varargs;
    PyObject_CallMethodStr: function ( obj : PPyObject; method, format, value : PAnsiChar ) : PPyObject; cdecl;
    PyObject_Compare: function (ob1,ob2:PPyObject):integer; cdecl;
    PyObject_RichCompare:function (ob1,ob2:PPyObject;opid:integer):PPyObject; cdecl;
    PyObject_RichCompareBool:function (ob1,ob2:PPyObject;opid:integer):Integer; cdecl;
    PyObject_GetAttr:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyObject_GetAttrString:function (ob:PPyObject;c:PAnsiChar):PPyObject; cdecl;
    PyObject_GetItem:function (ob,key:PPyObject):PPyObject; cdecl;
    PyObject_DelItem:function (ob,key:PPyObject):PPyObject; cdecl;
    PyObject_HasAttrString:function (ob:PPyObject;key:PAnsiChar):integer; cdecl;
    PyObject_Hash:function (ob:PPyObject):NativeInt; cdecl;
    PyObject_IsTrue:function (ob:PPyObject):integer; cdecl;
    PyObject_Length:function (ob:PPyObject):NativeInt; cdecl;
    PyObject_Repr:function (ob:PPyObject):PPyObject; cdecl;
    PyObject_SetAttr:function (ob1,ob2,ob3:PPyObject):integer; cdecl;
    PyObject_SetAttrString:function (ob:PPyObject;key:PAnsiChar;value:PPyObject):integer; cdecl;
    PyObject_SetItem:function (ob1,ob2,ob3:PPyObject):integer; cdecl;
    PyObject_Init:function (ob:PPyObject; t:PPyTypeObject):PPyObject; cdecl;
    PyObject_InitVar:function (ob:PPyObject; t:PPyTypeObject; size:NativeInt):PPyObject; cdecl;
    PyObject_New:function (t:PPyTypeObject):PPyObject; cdecl;
    PyObject_NewVar:function (t:PPyTypeObject; size:NativeInt):PPyObject; cdecl;
    PyObject_Free:procedure (ob:PPyObject); cdecl;
    PyObject_GetIter: function (obj: PPyObject) : PPyObject; cdecl;
    PyIter_Next: function (obj: PPyObject) : PPyObject; cdecl;
    PyObject_IsInstance:function (inst, cls:PPyObject):integer; cdecl;
    PyObject_IsSubclass:function (derived, cls:PPyObject):integer; cdecl;
    PyObject_Call:function (ob, args, kw:PPyObject):PPyObject; cdecl;
    PyObject_GenericGetAttr:function (obj, name : PPyObject) : PPyObject; cdecl;
    PyObject_GenericSetAttr:function (obj, name, value : PPyObject) : Integer; cdecl;
    PyObject_GC_Malloc:function (size:NativeUInt):PPyObject; cdecl;
    PyObject_GC_New:function (t:PPyTypeObject):PPyObject; cdecl;
    PyObject_GC_NewVar:function (t:PPyTypeObject; size:NativeInt):PPyObject; cdecl;
    PyObject_GC_Resize:function (t:PPyObject; newsize:NativeInt):PPyObject; cdecl;
    PyObject_GC_Del:procedure (ob:PPyObject); cdecl;
    PyObject_GC_Track:procedure (ob:PPyObject); cdecl;
    PyObject_GC_UnTrack:procedure (ob:PPyObject); cdecl;
    PySequence_Check:function (ob:PPyObject):integer; cdecl;
    PySequence_Concat:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PySequence_Count:function (ob1,ob2:PPyObject):integer; cdecl;
    PySequence_GetItem:function (ob:PPyObject;i:NativeInt):PPyObject; cdecl;
    PySequence_GetSlice:function (ob:PPyObject;i1,i2:NativeInt):PPyObject; cdecl;
    PySequence_In:function (ob1,ob2:PPyObject):integer; cdecl;
    PySequence_Index:function (ob1,ob2:PPyObject):NativeInt; cdecl;
    PySequence_Length:function (ob:PPyObject):NativeInt; cdecl;
    PySequence_Repeat:function (ob:PPyObject;count:NativeInt):PPyObject; cdecl;
    PySequence_SetItem:function (ob:PPyObject;i:NativeInt;value:PPyObject):integer; cdecl;
    PySequence_SetSlice:function (ob:PPyObject;i1,i2:NativeInt;value:PPyObject):integer; cdecl;
    PySequence_DelSlice:function (ob:PPyObject;i1,i2:NativeInt):integer; cdecl;
    PySequence_Tuple:function (ob:PPyObject):PPyObject; cdecl;
    PySequence_Contains:function (ob, value:PPyObject):integer; cdecl;
    PySeqIter_New: function(obj : PPyObject) : PPyObject; cdecl;
    PySlice_GetIndices:function (ob:PPySliceObject;length:NativeInt;var start,stop,step:NativeInt):integer; cdecl;
    PySlice_GetIndicesEx:function (ob:PPySliceObject;length:NativeInt;var start,stop,step,slicelength:NativeInt):integer; cdecl;
    PySlice_New:function (start,stop,step:PPyObject):PPyObject; cdecl;
    PyString_Concat:procedure(var ob1:PPyObject;ob2:PPyObject); cdecl;
    PyString_ConcatAndDel:procedure(var ob1:PPyObject;ob2:PPyObject); cdecl;
    PyString_Format:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyString_FromStringAndSize:function (s:PAnsiChar;i:NativeInt):PPyObject; cdecl;
    PyString_Size:function (ob:PPyObject):NativeInt; cdecl;
    PyString_DecodeEscape:function(s:PAnsiChar; len:NativeInt; errors:PAnsiChar; unicode:NativeInt; recode_encoding:PAnsiChar):PPyObject; cdecl;
    PyString_Repr:function(ob:PPyObject; smartquotes:integer):PPyObject; cdecl;
    PySys_GetObject:function (s:PAnsiChar):PPyObject; cdecl;
    //PySys_Init:procedure; cdecl;
    PySys_SetObject:function (s:PAnsiChar;ob:PPyObject):integer; cdecl;
    PySys_SetPath:procedure(path:PAnsiChar); cdecl;
    //PyTraceBack_Fetch:function :PPyObject; cdecl;
    PyTraceBack_Here:function (p:pointer):integer; cdecl;
    PyTraceBack_Print:function (ob1,ob2:PPyObject):integer; cdecl;
    //PyTraceBack_Store:function (ob:PPyObject):integer; cdecl;
    PyTuple_GetItem:function (ob:PPyObject;i:NativeInt):PPyObject; cdecl;
    PyTuple_GetSlice:function (ob:PPyObject;i1,i2:NativeInt):PPyObject; cdecl;
    PyTuple_New:function (size:NativeInt):PPyObject; cdecl;
    PyTuple_SetItem:function (ob:PPyObject;key:NativeInt;value:PPyObject):integer; cdecl;
    PyTuple_Size:function (ob:PPyObject):NativeInt; cdecl;
    PyType_IsSubtype:function (a, b : PPyTypeObject):integer; cdecl;
    PyType_GenericAlloc:function(atype: PPyTypeObject; nitems:NativeInt) : PPyObject; cdecl;
    PyType_GenericNew:function(atype: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;
    PyType_Ready:function(atype: PPyTypeObject) : integer; cdecl;
    PyUnicode_FromWideChar:function (const w:PWideChar; size:NativeInt):PPyObject; cdecl;
    PyUnicode_AsWideChar:function (unicode: PPyObject; w:PWideChar; size:NativeInt):integer; cdecl;
    PyUnicode_Decode:function (const s:PAnsiChar; size: NativeInt; const encoding : PAnsiChar; const errors: PAnsiChar):PPyObject; cdecl;
    PyUnicode_AsEncodedString:function (unicode:PPyObject; const encoding:PAnsiChar; const errors:PAnsiChar):PPyObject; cdecl;
    PyUnicode_FromOrdinal:function (ordinal:integer):PPyObject; cdecl;
    PyWeakref_GetObject: function ( ref : PPyObject) : PPyObject; cdecl;
    PyWeakref_NewProxy: function ( ob, callback : PPyObject) : PPyObject; cdecl;
    PyWeakref_NewRef: function ( ob, callback : PPyObject) : PPyObject; cdecl;
    PyWrapper_New: function ( ob1, ob2 : PPyObject) : PPyObject; cdecl;
    PyBool_FromLong: function ( ok : Integer) : PPyObject; cdecl;
    PyThreadState_SetAsyncExc: function(t_id :LongInt; exc :PPyObject) : Integer; cdecl;
    Py_AtExit:function (proc: AtExitProc):integer; cdecl;
    //Py_Cleanup:procedure; cdecl;
    Py_CompileString:function (s1,s2:PAnsiChar;i:integer):PPyObject; cdecl;
    Py_FatalError:procedure(s:PAnsiChar); cdecl;
    Py_FindMethod:function (md:PPyMethodDef;ob:PPyObject;key:PAnsiChar):PPyObject; cdecl;
    Py_FindMethodInChain:function (mc:PPyMethodChain;ob:PPyObject;key:PAnsiChar):PPyObject; cdecl;
    _PyObject_New:function (obt:PPyTypeObject;ob:PPyObject):PPyObject; cdecl;
    _PyString_Resize:function (var ob:PPyObject;i:NativeInt):integer; cdecl;
    Py_Finalize                     : procedure; cdecl;
    PyErr_ExceptionMatches          : function ( exc : PPyObject) : Integer; cdecl;
    PyErr_GivenExceptionMatches     : function ( raised_exc, exc : PPyObject) : Integer; cdecl;
    PyEval_EvalCode                 : function ( co : PPyCodeObject; globals, locals : PPyObject) : PPyObject; cdecl;
    Py_GetVersion                   : function : PAnsiChar; cdecl;
    Py_GetCopyright                 : function : PAnsiChar; cdecl;
    Py_GetExecPrefix                : function : PAnsiChar; cdecl;
    Py_GetPath                      : function : PAnsiChar; cdecl;
    Py_GetPrefix                    : function : PAnsiChar; cdecl;
    Py_GetProgramName               : function : PAnsiChar; cdecl;

    PyParser_SimpleParseString      : function ( str : PAnsiChar; start : Integer) : PNode; cdecl;
    PyNode_Free                     : procedure( n : PNode ); cdecl;
    PyErr_NewException              : function ( name : PAnsiChar; base, dict : PPyObject ) : PPyObject; cdecl;
    Py_Malloc                       : function ( size : NativeInt ) : Pointer;
    PyMem_Malloc                    : function ( size : NativeInt ) : Pointer;

{New exported Objects in Python 1.5}
    Py_SetProgramName               : procedure( name: PAnsiChar); cdecl;
    Py_SetProgramName3000           : procedure( name: PWideChar); cdecl;
    Py_IsInitialized                : function : integer; cdecl;
    Py_GetProgramFullPath           : function : PAnsiChar; cdecl;
    Py_NewInterpreter               : function : PPyThreadState; cdecl;
    Py_EndInterpreter               : procedure( tstate: PPyThreadState); cdecl;
    PyEval_AcquireLock              : procedure; cdecl;
    PyEval_ReleaseLock              : procedure; cdecl;
    PyEval_AcquireThread            : procedure( tstate: PPyThreadState); cdecl;
    PyEval_ReleaseThread            : procedure( tstate: PPyThreadState); cdecl;
    PyInterpreterState_New          : function : PPyInterpreterState; cdecl;
    PyInterpreterState_Clear        : procedure( interp: PPyInterpreterState); cdecl;
    PyInterpreterState_Delete       : procedure( interp: PPyInterpreterState); cdecl;
    PyThreadState_New               : function ( interp: PPyInterpreterState): PPyThreadState; cdecl;
    PyThreadState_Clear             : procedure( tstate: PPyThreadState); cdecl;
    PyThreadState_Delete            : procedure( tstate: PPyThreadState); cdecl;
    PyThreadState_Get               : function : PPyThreadState; cdecl;
    PyThreadState_Swap              : function ( tstate: PPyThreadState): PPyThreadState; cdecl;
    PyErr_SetInterrupt              : procedure; cdecl;
    PyGILState_Ensure               : function() : PyGILstate_STATE; cdecl;
    PyGILState_Release              : procedure(gilstate : PyGILState_STATE); cdecl;
{Further exported Objects, may be implemented later}
{
    PyCode_New: Pointer;
    PyFile_AsFile: Pointer;
    PyFile_FromFile: Pointer;
    PyFloat_AsString: Pointer;
    PyFrame_BlockPop: Pointer;
    PyFrame_BlockSetup: Pointer;
    PyFrame_ExtendStack: Pointer;
    PyFrame_FastToLocals: Pointer;
    PyFrame_LocalsToFast: Pointer;
    PyFrame_New: Pointer;
    PyGrammar_AddAccelerators: Pointer;
    PyGrammar_FindDFA: Pointer;
    PyGrammar_LabelRepr: Pointer;
    PyInstance_DoBinOp: Pointer;
    PyInt_GetMax: Pointer;
    PyMarshal_Init: Pointer;
    PyMarshal_ReadLongFromFile: Pointer;
    PyMarshal_ReadObjectFromFile: Pointer;
    PyMarshal_ReadObjectFromString: Pointer;
    PyMarshal_WriteLongToFile: Pointer;
    PyMarshal_WriteObjectToFile: Pointer;
    PyMember_Get: Pointer;
    PyMember_Set: Pointer;
    PyNode_AddChild: Pointer;
    PyNode_Compile: Pointer;
    PyNode_New: Pointer;
    PyOS_GetLastModificationTime: Pointer;
    PyOS_Readline: Pointer;
    PyOS_strtol: Pointer;
    PyOS_strtoul: Pointer;
    PyObject_CallFunction: Pointer;
    PyObject_Print: Pointer;
    PyParser_AddToken: Pointer;
    PyParser_Delete: Pointer;
    PyParser_New: Pointer;
    PyParser_ParseFile: Pointer;
    PyParser_ParseString: Pointer;
    PyParser_SimpleParseFile: Pointer;
    PyRun_AnyFile: Pointer;
    PyRun_File: Pointer;
    PyRun_InteractiveLoop: Pointer;
    PyRun_InteractiveOne: Pointer;
    PyRun_SimpleFile: Pointer;
    PySys_GetFile: Pointer;
    PyToken_OneChar: Pointer;
    PyToken_TwoChars: Pointer;
    PyTokenizer_Free: Pointer;
    PyTokenizer_FromFile: Pointer;
    PyTokenizer_FromString: Pointer;
    PyTokenizer_Get: Pointer;
    Py_Main: Pointer;
    _PyParser_Grammar: Pointer;
    _PyParser_TokenNames: Pointer;
    _PyThread_Started: Pointer;
    _Py_c_diff: Pointer;
    _Py_c_neg: Pointer;
    _Py_c_pow: Pointer;
    _Py_c_prod: Pointer;
    _Py_c_quot: Pointer;
    _Py_c_sum: Pointer;
}
  // functions redefined in Delphi
  procedure   Py_INCREF   ( op: PPyObject);
  procedure   Py_DECREF   ( op: PPyObject);
  procedure   Py_XINCREF  ( op: PPyObject);
  procedure   Py_XDECREF  ( op: PPyObject);

  function Py_GetPlatform: PAnsiChar; cdecl;
  function PyCode_Addr2Line( co: PPyCodeObject; addrq : Integer ) : Integer; cdecl;
  function Py_GetBuildInfo: PAnsiChar; cdecl;
  function PyImport_ExecCodeModule( const AName : AnsiString; codeobject : PPyObject) : PPyObject;
  function PyString_Check( obj : PPyObject ) : Boolean;
  function PyString_CheckExact( obj : PPyObject ) : Boolean;
  function PyFloat_Check( obj : PPyObject ) : Boolean;
  function PyFloat_CheckExact( obj : PPyObject ) : Boolean;
  function PyInt_Check( obj : PPyObject ) : Boolean;
  function PyInt_CheckExact( obj : PPyObject ) : Boolean;
  function PyLong_Check( obj : PPyObject ) : Boolean;
  function PyLong_CheckExact( obj : PPyObject ) : Boolean;
  function PyTuple_Check( obj : PPyObject ) : Boolean;
  function PyTuple_CheckExact( obj : PPyObject ) : Boolean;
  function PyInstance_Check( obj : PPyObject ) : Boolean;
  function PyClass_Check( obj : PPyObject ) : Boolean;
  function PyType_CheckExact( obj : PPyObject ) : Boolean;
  function PyMethod_Check( obj : PPyObject ) : Boolean;
  function PyList_Check( obj : PPyObject ) : Boolean;
  function PyList_CheckExact( obj : PPyObject ) : Boolean;
  function PyDict_Check( obj : PPyObject ) : Boolean;
  function PyDict_CheckExact( obj : PPyObject ) : Boolean;
  function PyModule_Check( obj : PPyObject ) : Boolean;
  function PyModule_CheckExact( obj : PPyObject ) : Boolean;
  function PySlice_Check( obj : PPyObject ) : Boolean;
  function PyFunction_Check( obj : PPyObject ) : Boolean;
  function PyIter_Check( obj : PPyObject ) : Boolean;
  function PyUnicode_Check( obj : PPyObject ) : Boolean;
  function PyUnicode_CheckExact( obj : PPyObject ) : Boolean;
  function PyType_IS_GC(t : PPyTypeObject ) : Boolean;
  function PyObject_IS_GC( obj : PPyObject ) : Boolean;
  function PyWeakref_Check( obj : PPyObject ) : Boolean;
  function PyWeakref_CheckRef( obj : PPyObject ) : Boolean;
  function PyWeakref_CheckProxy( obj : PPyObject ) : Boolean;
  function PyBool_Check( obj : PPyObject ) : Boolean;
  function PyBaseString_Check( obj : PPyObject ) : Boolean;
  function PyEnum_Check( obj : PPyObject ) : Boolean;
  function PyObject_TypeCheck(obj:PPyObject; t:PPyTypeObject) : Boolean;
  function Py_InitModule( const AName : PAnsiChar; md : PPyMethodDef) : PPyObject;
  function Py_InitModule3000( const md : PyModuleDef) : PPyObject;
  function PyString_FromString( str: PAnsiChar): PPyObject; virtual; abstract;
  function PyString_AsDelphiString( ob: PPyObject): string;  virtual; abstract;
  procedure Py_FlushLine; cdecl;

  // Constructors & Destructors
  constructor Create(AOwner: TComponent); override;

  // Public methods
  procedure MapDll;

  // Public properties
  property Initialized : Boolean read GetInitialized;
  property Finalizing : Boolean read FFinalizing;
  property IsPython3000 : Boolean read FIsPython3000;
  property MajorVersion : integer read FMajorVersion;
  property MinorVersion : integer read FMinorVersion;
  property BuiltInModuleName: String read FBuiltInModuleName write FBuiltInModuleName;

end;

//--------------------------------------------------------
//--                                                    --
//-- class:  TPythonEngine derived from TPythonInterface--
//-- Pytrunobject providing interface for               --
//-- running Python into Delphi                         --
//--------------------------------------------------------
type
  TDatetimeConversionMode = (dcmToTuple, dcmToDatetime);
const
  DEFAULT_DATETIME_CONVERSION_MODE = dcmToTuple;
type
  TEngineClient = class;
  TPathInitializationEvent = procedure ( Sender : TObject; var Path : String ) of Object;
  TSysPathInitEvent = procedure ( Sender : TObject; PathList : PPyObject ) of Object;
  TPythonFlag = (pfDebug, pfInteractive, pfNoSite, pfOptimize, pfTabcheck, pfUnicode, pfVerbose,
                 pfUseClassExceptionsFlag, pfFrozenFlag, pfIgnoreEnvironmentFlag, pfDivisionWarningFlag);
  TPythonFlags = set of TPythonFlag;


  TTracebackItem = class
  public
    FileName : String;
    LineNo : Integer;
    Context : String;
  end;

  TPythonTraceback = class
    protected
      FItems : TList;
      FLimit : Integer;

      function GetItemCount : Integer;
      function GetItem( idx : Integer ) : TTracebackItem;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Clear;
      procedure Refresh;

      property ItemCount : Integer read GetItemCount;
      property Items[ idx : Integer ] : TTracebackItem read GetItem;
      property Limit : Integer read FLimit write FLimit;
  end;

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonEngine = class(TPythonInterface)
  private
    FInitScript:                 TStrings;
    FIO:                         TPythonInputOutput;
    FRedirectIO:                 Boolean;
    FOnAfterInit:                TNotifyEvent;
    FClients:                    TList;
    FLock:                       TCriticalSection;
    FExecModule:                 AnsiString;
    FAutoFinalize:               Boolean;
    FProgramName:                AnsiString;
    FProgramNameW:               UnicodeString;
    FInitThreads:                Boolean;
    FOnPathInitialization:       TPathInitializationEvent;
    FOnSysPathInit:              TSysPathInitEvent;
    FTraceback:                  TPythonTraceback;
    FUseWindowsConsole:          Boolean;
    FGlobalVars:                 PPyObject;
    FLocalVars:                  PPyObject;
    FPyFlags:                    TPythonFlags;
    FIORedirected:               Boolean;
    FIOPythonModule:             TObject;
    FDatetimeConversionMode:     TDatetimeConversionMode;
    FTimeStruct:                 PPyObject;
    FPyDateTime_DateType:        PPyObject;
    FPyDateTime_DateTimeType:    PPyObject;
    FPyDateTime_DeltaType:       PPyObject;
    FPyDateTime_TimeType:        PPyObject;
    FPyDateTime_TZInfoType:      PPyObject;
    FPyDateTime_TimeTZType:      PPyObject;
    FPyDateTime_DateTimeTZType:  PPyObject;
    function  GetVersion: String;
    procedure SetVersion(const Value: String);

  protected
    procedure AfterLoad; override;
    procedure BeforeLoad; override;
    procedure DoOpenDll(const aDllName : String); override;
    procedure SetInitScript(Value: TStrings);
    function  GetThreadState: PPyThreadState;
    function  GetInterpreterState: PPyInterpreterState;
    procedure SetInitThreads(Value: Boolean);
    function  GetClientCount : Integer;
    function  GetClients( idx : Integer ) : TEngineClient;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CheckRegistry;
    procedure SetProgramArgs;
    procedure InitWinConsole;
    procedure SetUseWindowsConsole( const Value : Boolean );
    procedure SetGlobalVars(const Value: PPyObject);
    procedure SetLocalVars(const Value: PPyObject);
    procedure SetPyFlags(const Value: TPythonFlags);
    procedure AssignPyFlags;

  public
    // Constructors & Destructors
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    // Public methods
    procedure  Initialize;
    procedure  Finalize;
    procedure  Lock;
    procedure  Unlock;
    function   IsType(ob: PPyObject; obt: PPyTypeObject): Boolean;
    function   GetAttrString(obj: PPyObject; AName: PAnsiChar):PAnsiChar;
    function   CleanString(const s : AnsiString) : AnsiString;
    function   Run_CommandAsString(const command : AnsiString; mode : Integer) : String;
    function   Run_CommandAsObject(const command : AnsiString; mode : Integer) : PPyObject;
    function   Run_CommandAsObjectWithDict(const command : AnsiString; mode : Integer; locals, globals : PPyObject) : PPyObject;
    procedure  ExecString(const command : AnsiString); overload;
    procedure  ExecStrings( strings : TStrings ); overload;
    function   EvalString(const command : AnsiString) : PPyObject; overload;
    function   EvalStringAsStr(const command : AnsiString) : String;
    function   EvalStrings( strings : TStrings ) : PPyObject; overload;
    procedure  ExecString(const command : AnsiString; locals, globals : PPyObject ); overload;
    procedure  ExecStrings( strings : TStrings; locals, globals : PPyObject ); overload;
    function   EvalString( const command : AnsiString; locals, globals : PPyObject ) : PPyObject; overload;
    function   EvalStrings( strings : TStrings; locals, globals : PPyObject ) : PPyObject; overload;
    function   EvalStringsAsStr( strings : TStrings ) : String;
    function   EvalPyFunction(pyfunc, pyargs:PPyObject): Variant;
    function   EvalFunction(pyfunc:PPyObject; args: array of const): Variant;
    function   EvalFunctionNoArgs(pyfunc:PPyObject): Variant;
    function   CheckEvalSyntax( const str : AnsiString ) : Boolean;
    function   CheckExecSyntax( const str : AnsiString ) : Boolean;
    function   CheckSyntax( const str : AnsiString; mode : Integer ) : Boolean;
    procedure  RaiseError;
    function   PyObjectAsString( obj : PPyObject ) : String;
    procedure  DoRedirectIO;
    procedure  AddClient( client : TEngineClient );
    procedure  RemoveClient( client : TEngineClient );
    function   FindClient( const aName : string ) : TEngineClient;
    function   TypeByName( const aTypeName : AnsiString ) : PPyTypeObject;
    function   ModuleByName( const aModuleName : AnsiString ) : PPyObject;
    function   MethodsByName( const aMethodsContainer: string ) : PPyMethodDef;
    function   VariantAsPyObject( const V : Variant ) : PPyObject; virtual;
    function   PyObjectAsVariant( obj : PPyObject ) : Variant; virtual;
    function   VarRecAsPyObject( v : TVarRec ) : PPyObject;
    function   MakePyTuple( const objects : array of PPyObject ) : PPyObject;
    function   MakePyList( const objects : array of PPyObject ) : PPyObject;
    function   ArrayToPyTuple( items : array of const) : PPyObject;
    function   ArrayToPyList( items : array of const) : PPyObject;
    function   ArrayToPyDict( items : array of const) : PPyObject;
    function   StringsToPyList( strings : TStrings ) : PPyObject;
    function   StringsToPyTuple( strings : TStrings ) : PPyObject;
    procedure  PyListToStrings( list : PPyObject; strings : TStrings );
    procedure  PyTupleToStrings( tuple: PPyObject; strings : TStrings );
    function   PyUnicode_AsWideString( obj : PPyObject ) : UnicodeString;
    function   PyUnicode_FromWideString( const AString : UnicodeString) : PPyObject;
    function   ReturnNone : PPyObject;
    function   FindModule( const ModuleName : AnsiString ) : PPyObject;
    function   FindFunction(ModuleName,FuncName: AnsiString): PPyObject;
    function   SetToList( data : Pointer; size : Integer ) : PPyObject;
    procedure  ListToSet( List : PPyObject; data : Pointer; size : Integer );
    procedure  CheckError(ACatchStopEx : Boolean = False);
    function   GetMainModule : PPyObject;
    function   PyTimeStruct_Check( obj : PPyObject ) : Boolean;
    { Date, Time, DateTime and related objects check functions }
    function   PyDate_Check( obj : PPyObject ) : Boolean;
    function   PyDate_CheckExact( obj : PPyObject ) : Boolean;
    function   PyDateTime_Check( obj : PPyObject ) : Boolean;
    function   PyDateTime_CheckExact( obj : PPyObject ) : Boolean;
    function   PyTime_Check( obj : PPyObject ) : Boolean;
    function   PyTime_CheckExact( obj : PPyObject ) : Boolean;
    function   PyDelta_Check( obj : PPyObject ) : Boolean;
    function   PyDelta_CheckExact( obj : PPyObject ) : Boolean;
    function   PyTZInfo_Check( obj : PPyObject ) : Boolean;
    function   PyTZInfo_CheckExact( obj : PPyObject ) : Boolean;
    { end date/time functions }
    function PyString_FromString( str: PAnsiChar): PPyObject; override;
    function PyString_AsDelphiString( ob: PPyObject): string; override;
    function PyString_AsAnsiString( ob: PPyObject): AnsiString;
    function PyString_AsWideString( ob: PPyObject): UnicodeString;

    // Public Properties
    property ClientCount : Integer read GetClientCount;
    property Clients[ idx : Integer ] : TEngineClient read GetClients;
    property ExecModule : AnsiString read FExecModule write FExecModule;
    property ThreadState: PPyThreadState read GetThreadState;
    property InterpreterState: PPyInterpreterState read GetInterpreterState;
    property Traceback : TPythonTraceback read FTraceback;
    property LocalVars : PPyObject read FLocalVars Write SetLocalVars;
    property GlobalVars : PPyObject read FGlobalVars Write SetGlobalVars;
    property IOPythonModule: TObject read FIOPythonModule; {TPythonModule}
  published
    property AutoFinalize: Boolean read FAutoFinalize write FAutoFinalize default True;
    property DatetimeConversionMode: TDatetimeConversionMode read FDatetimeConversionMode write FDatetimeConversionMode default DEFAULT_DATETIME_CONVERSION_MODE;
    property InitScript: TStrings read FInitScript write SetInitScript;
    property InitThreads: Boolean read FInitThreads write SetInitThreads default False;
    property IO: TPythonInputOutput read FIO write FIO;
    property PyFlags: TPythonFlags read FPyFlags write SetPyFlags default [];
    property RedirectIO: Boolean read FRedirectIO write FRedirectIO default True;
    property UseWindowsConsole: Boolean read FUseWindowsConsole write FUseWindowsConsole default False;
    property Version : String read GetVersion write SetVersion stored False;
    property OnAfterInit: TNotifyEvent read FOnAfterInit write FOnAfterInit;
    property OnPathInitialization: TPathInitializationEvent read FOnPathInitialization write FOnPathInitialization;
    property OnSysPathInit: TSysPathInitEvent read FOnSysPathInit write FOnSysPathInit;
  end;


//-------------------------------------------------------
//--                                                   --
//--      Base class:  TEngineClient                   --
//--                                                   --
//-------------------------------------------------------

  TEngineClient = class(TComponent)
    protected
      FEngine : TPythonEngine;
      FOnInitialization : TNotifyEvent;
      FOnFinalization : TNotifyEvent;
      FOnCreate : TNotifyEvent;
      FOnDestroy : TNotifyEvent;
      FInitialized : Boolean;

      procedure SetEngine( val : TPythonEngine ); virtual;
      procedure Loaded; override;
      procedure Notification( AComponent: TComponent;
                              Operation: TOperation); override;
      procedure ModuleReady(Sender : TObject); virtual;
    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // Public Methods
      procedure Initialize; virtual;
      procedure Finalize; virtual;
      procedure ClearEngine;
      procedure CheckEngine;

      // Public Properties
      property Initialized: Boolean read FInitialized;

    published
      property Engine : TPythonEngine read FEngine write SetEngine;
      property OnCreate : TNotifyEvent read FOnCreate write FOnCreate;
      property OnDestroy : TNotifyEvent read FOnDestroy write FOnDestroy;
      property OnFinalization : TNotifyEvent read FOnFinalization write FOnFinalization;
      property OnInitialization : TNotifyEvent read FOnInitialization write FOnInitialization;
  end;

//-------------------------------------------------------
//--                                                   --
//--class: TMethodsContainer derived from TEngineClient--
//--                                                   --
//-------------------------------------------------------

  TMethodArray = array[ 0 .. 16000 ] of PyMethodDef;
  PMethodArray = ^TMethodArray;
  TDelphiMethod = function ( self, args : PPyObject ) : PPyObject of object; cdecl;
  TDelphiMethodWithKW = function ( self, args, keywords : PPyObject ) : PPyObject of object; cdecl;
  TPythonEvent = procedure(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject) of object;
  TMethodsContainer = class; // forward declaration
  TEventDefs = class; // forward declaration

  // Event Collection Item
  TEventDef = class(TCollectionItem)
  private
    FName: AnsiString;
    FTmpDocString: AnsiString;
    FOnExecute: TPythonEvent;
    FDocString: TStringList;
 	 procedure SetDocString(const Value: TStringList);
  protected
    function  GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function  GetDocString : AnsiString;
    function  PythonEvent(pself, args: PPyObject): PPyObject; cdecl;
    function  Owner : TEventDefs;
  published
    property Name: string read GetDisplayName write SetDisplayName;
    property OnExecute: TPythonEvent read FOnExecute write FOnExecute;
    property DocString: TStringList read FDocString write SetDocString;
  end;

  // Event Collection
  TEventDefs = class(TCollection)
  protected
    FMethodsContainer : TMethodsContainer;

    function  GetItems( idx : Integer ) : TEventDef;
    procedure SetItems( idx : Integer; Value : TEventDef );
    function  GetOwner: TPersistent; override;
  public
    constructor Create( AMethodsContainer : TMethodsContainer );

    function  Add : TEventDef;
    procedure RegisterEvents;

    property Items[ idx : Integer ] : TEventDef read GetItems;
    property Container : TMethodsContainer read FMethodsContainer;
  end;

  // class TMethodsContainer
  TMethodsContainer = class(TEngineClient)
    private
      FMethodCount : Integer;
      FAllocatedMethodCount : Integer;
      FMethods : PPyMethodDef;
      FModuleDef : PyModuleDef;  // for Python 3000
      FEventDefs: TEventDefs;

      procedure AllocMethods;
      procedure FreeMethods;
      function  GetMethods( idx : Integer ) : PPyMethodDef;
      function  StoreEventDefs: Boolean;

    protected
      procedure ReallocMethods; virtual;

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // public methods
      procedure Initialize; override;
      procedure Finalize; override;

      function  AddMethod( AMethodName  : PAnsiChar;
                           AMethod  : PyCFunction;
                           ADocString : PAnsiChar ) : PPyMethodDef;
      function  AddMethodWithKeywords( AMethodName  : PAnsiChar;
                                       AMethod  : PyCFunctionWithKW;
                                       ADocString : PAnsiChar ) : PPyMethodDef;
      function  AddDelphiMethod( AMethodName  : PAnsiChar;
                                 ADelphiMethod: TDelphiMethod;
                                 ADocString : PAnsiChar ) : PPyMethodDef;
      function  AddDelphiMethodWithKeywords(  AMethodName  : PAnsiChar;
                                              ADelphiMethod: TDelphiMethodWithKW;
                                              ADocString : PAnsiChar ) : PPyMethodDef;
      procedure ClearMethods;

      // properties
      property MethodCount : Integer read FMethodCount;
      property Methods[ idx : Integer ] : PPyMethodDef read GetMethods;
      property MethodsData : PPyMethodDef read FMethods;
      property ModuleDef : PyModuleDef read FModuleDef;

    published
      property Events: TEventDefs read fEventDefs write fEventDefs stored StoreEventDefs;
  end;


//------------------------------------------------------------
//--                                                        --
//--class: TMembersContainer derived from TMethodsContainer --
//--                                                        --
//------------------------------------------------------------

  TMemberArray = array[ 0 .. 16000 ] of PyMemberDef;
  PMemberArray = ^TMemberArray;

  // class TMembersContainer
  TMembersContainer = class(TMethodsContainer)
    protected
      function  GetMembersStartOffset : Integer; virtual;
    private
      FMemberCount : Integer;
      FAllocatedMemberCount : Integer;
      FMembers : PPyMemberDef;

      procedure AllocMembers;
      procedure FreeMembers;
      function  GetMembers( idx : Integer ) : PPyMemberDef;

    protected
      procedure ReallocMembers; virtual;

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // public methods
      procedure AddMember( MemberName  : PAnsiChar;
                           MemberType  : TPyMemberType;
                           MemberOffset : NativeInt;
                           MemberFlags : TPyMemberFlag;
                           MemberDoc : PAnsiChar );
      procedure ClearMembers;
      procedure Finalize; override;

      // properties
      property MemberCount : Integer read FMemberCount;
      property Members[ idx : Integer ] : PPyMemberDef read GetMembers;
      property MembersData : PPyMemberDef read FMembers;
  end;

//------------------------------------------------------------
//--                                                        --
//--class: TGetSetContainer derived from TMembersContainer  --
//--                                                        --
//------------------------------------------------------------

  TGetSetArray = array[ 0 .. 16000 ] of PyGetSetDef;
  PGetSetArray = ^TGetSetArray;

  // class TGetSetContainer
  TGetSetContainer = class(TMembersContainer)
    private
      FGetSetCount : Integer;
      FAllocatedGetSetCount : Integer;
      FGetSets : PPyGetSetDef;

      procedure AllocGetSets;
      procedure FreeGetSets;
      function  GetGetSet( idx : Integer ) : PPyGetSetDef;

    protected
      procedure ReallocGetSets; virtual;

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // public methods
      procedure AddGetSet( AName  : PAnsiChar;
                           AGet : getter;
                           ASet : setter;
                           ADoc : PAnsiChar;
                           AClosure : Pointer);
      procedure ClearGetSets;
      procedure Finalize; override;

      // properties
      property GetSetCount : Integer read FGetSetCount;
      property GetSet[ idx : Integer ] : PPyGetSetDef read GetGetSet;
      property GetSetData : PPyGetSetDef read FGetSets;
  end;

//-------------------------------------------------------
//--                                                   --
//--class: TPythonModule derived from TMethodsContainer--
//--                                                   --
//-------------------------------------------------------

  TPythonModule = class; // forward declaration
  TErrors = class; // forward declaration

  TErrorType = (etString, etClass);

  TParentClassError = class(TPersistent)
    protected
      FName : AnsiString;
      FModule : AnsiString;
    public
      procedure AssignTo( Dest: TPersistent ); override;
    published
      property Module : AnsiString read FModule write FModule;
      property Name : AnsiString read FName write FName;
  end;

  TError = class(TCollectionItem)
  protected
    FName        : AnsiString;
    FText        : AnsiString;
    FError       : PPyObject;
    FErrorType   : TErrorType;
    FParentClass : TParentClassError;

    function GetDisplayName: string; override;
    procedure SetName( const Value : AnsiString );
    procedure SetText( const Value : AnsiString );
    procedure SetErrorType( Value : TErrorType );
    procedure SetParentClass( Value : TParentClassError );
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildError( const ModuleName : AnsiString );
    procedure RaiseError( const msg : AnsiString );
    procedure RaiseErrorObj( const msg : AnsiString; obj : PPyObject );
    function  Owner : TErrors;
    property Error : PPyObject read FError write FError;
  published
    property Name : AnsiString read FName write SetName;
    property Text : AnsiString read FText write SetText;
    property ErrorType : TErrorType read FErrorType write SetErrorType;
    property ParentClass : TParentClassError read FParentClass write SetParentClass;
  end;

  TErrors = class(TCollection)
  private
    FModule: TPythonModule;
    function GetError(Index: Integer): TError;
    procedure SetError(Index: Integer; Value: TError);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Module: TPythonModule);
    function  Add: TError;
    function  Owner : TPythonModule;
    property Items[Index: Integer]: TError read GetError write SetError; default;
  end;

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonModule = class(TMethodsContainer)
    protected
      FModuleName : AnsiString;
      FModule : PPyObject;
      FClients : TList;
      FErrors : TErrors;
      FOnAfterInitialization : TNotifyEvent;
      FDocString : TStringList;

      function GetClientCount : Integer;
      function GetClients( idx : Integer ) : TEngineClient;
      procedure SetErrors( val : TErrors );
      procedure SetModuleName( const val : AnsiString );
      procedure SetDocString( value : TStringList );

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // Public methods
      procedure MakeModule;
      procedure DefineDocString;
      procedure Initialize; override;
      procedure InitializeForNewInterpreter;
      procedure AddClient( client : TEngineClient );
      function  ErrorByName( const AName : AnsiString ) : TError;
      procedure RaiseError( const error, msg : AnsiString );
      procedure RaiseErrorFmt( const error, format : AnsiString; Args : array of const );
      procedure RaiseErrorObj( const error, msg : AnsiString; obj : PPyObject );
      procedure BuildErrors;
      procedure SetVar( const varName : AnsiString; value : PPyObject );
      function  GetVar( const varName : AnsiString ) : PPyObject;
      procedure DeleteVar( const varName : AnsiString );
      procedure SetVarFromVariant( const varName : AnsiString; const value : Variant );
      function  GetVarAsVariant( const varName: AnsiString ) : Variant;

      // Public properties
      property Module : PPyObject read FModule;
      property Clients[ idx : Integer ] : TEngineClient read GetClients;
      property ClientCount : Integer read GetClientCount;

    published
      property DocString : TStringList read FDocString write SetDocString;
      property ModuleName : AnsiString read FModuleName write SetModuleName;
      property Errors : TErrors read FErrors write SetErrors;
      property OnAfterInitialization : TNotifyEvent read FOnAfterInitialization write FOnAfterInitialization;
  end;


//-------------------------------------------------------
//--                                                   --
//--class:  TPythonType  derived from TGetSetContainer --
//--                                                   --
//-------------------------------------------------------

type
  TPythonType = class; //forward declaration

{
        A                    B                                                      C
        +-------------------++------------------------------------------------------+
        | PyObject header   ||             TPyObject class                          |
        +----------+--------++-----------------+------------+----------+------------+
        |ob_refcnt |ob_type ||hidden Class Ptr |PythonType  |IsSubType |PythonAlloc |
        |integer   |pointer ||pointer          |TPythonType |Boolean   |Boolean     |
        |4 bytes   |4 bytes ||4 bytes          |4 bytes     |1 byte    |1 byte      |
        +----------+--------++-----------------+------------+----------+------------+

        ^                    ^
        |                    |
        ptr returned         ptr returned by Adjust
        by GetSelf

        - a Python object must start at A.
        - a Delphi class class must start at B
        - TPyObject.InstanceSize will return C-B
        - Sizeof(TPyObject) will return C-B
        - The total memory allocated for a TPyObject instance will be C-A,
          even if its InstanceSize is C-B.
        - When turning a Python object pointer into a Delphi instance pointer, PythonToDelphi
          will offset the pointer from A to B.
        - When turning a Delphi instance into a Python object pointer, GetSelf will offset
          Self from B to A.
        - Properties ob_refcnt and ob_type will call GetSelf to access their data.
}
  // The base class of all new Python types
  TPyObject = class
  private
    function  Get_ob_refcnt: NativeInt;
    function  Get_ob_type: PPyTypeObject;
    procedure Set_ob_refcnt(const Value: NativeInt);
    procedure Set_ob_type(const Value: PPyTypeObject);
  public
    PythonType     : TPythonType;
    IsSubtype      : Boolean;
    PythonAlloc    : Boolean;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); virtual;
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); virtual;
    destructor  Destroy; override;

    class function NewInstance: TObject; override;
    procedure FreeInstance; override;

    // Misc
    function  GetSelf : PPyObject;
    procedure IncRef;
    procedure Adjust(PyPointer: Pointer);
    function  GetModule : TPythonModule;

    property ob_refcnt : NativeInt read Get_ob_refcnt write Set_ob_refcnt;
    property ob_type   : PPyTypeObject read Get_ob_type write Set_ob_type;

    // Type services
    ////////////////

    // Basic services
    function  Print( var f: file; i: integer) : Integer; virtual;
    function  GetAttr(key : PAnsiChar) : PPyObject; virtual;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; virtual;
    function  Repr : PPyObject; virtual;
    function  Compare( obj: PPyObject) : Integer; virtual;
    function  Hash : NativeInt; virtual;
    function  Str: PPyObject; virtual;
    function  GetAttrO( key: PPyObject) : PPyObject; virtual;
    function  SetAttrO( key, value: PPyObject) : Integer; virtual;
    function  Call( ob1, ob2 : PPyObject) : PPyObject; virtual;
    function  Traverse( proc: visitproc; ptr: Pointer) : integer; virtual;
    function  Clear: integer; virtual;
    function  RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject; virtual;
    function  Iter : PPyObject; virtual;
    function  IterNext : PPyObject; virtual;
    function  Init( args, kwds : PPyObject ) : Integer; virtual;

    // Number services
    function  NbAdd( obj : PPyObject) : PPyObject; virtual;
    function  NbSubstract( obj : PPyObject) : PPyObject; virtual;
    function  NbMultiply( obj : PPyObject) : PPyObject; virtual;
    function  NbDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbFloorDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbTrueDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbRemainder( obj : PPyObject) : PPyObject; virtual;
    function  NbDivmod( obj : PPyObject) : PPyObject; virtual;
    function  NbPower( ob1, ob2 : PPyObject) : PPyObject; virtual;
    function  NbNegative : PPyObject; virtual;
    function  NbPositive : PPyObject; virtual;
    function  NbAbsolute : PPyObject; virtual;
    function  NbNonZero : Integer; virtual;
    function  NbInvert : PPyObject; virtual;
    function  NbLShift( obj : PPyObject) : PPyObject; virtual;
    function  NbRShift( obj : PPyObject) : PPyObject; virtual;
    function  NbAnd( obj : PPyObject) : PPyObject; virtual;
    function  NbXor( obj : PPyObject) : PPyObject; virtual;
    function  NbOr( obj : PPyObject) : PPyObject; virtual;
    function  NbCoerce( obj : PPPyObject) : Integer; virtual;
    function  NbInt : PPyObject; virtual;
    function  NbLong : PPyObject; virtual;
    function  NbFloat : PPyObject; virtual;
    function  NbOct : PPyObject; virtual;
    function  NbHex : PPyObject; virtual;
    function  NbInplaceAdd( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceSubtract( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceMultiply( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceDivide( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceFloorDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbInplaceTrueDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbInplaceRemainder( obj : PPyObject): PPyObject; virtual;
    function  NbInplacePower( ob1, ob2 : PPyObject): PPyObject; virtual;
    function  NbInplaceLshift( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceRshift( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceAnd( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceXor( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceOr( obj : PPyObject): PPyObject; virtual;
    // Sequence services
    function  SqLength : NativeInt; virtual;
    function  SqConcat( obj : PPyObject) : PPyObject; virtual;
    function  SqRepeat( val : NativeInt ) : PPyObject; virtual;
    function  SqItem( idx : NativeInt ) : PPyObject; virtual;
    function  SqSlice( idx1, idx2 : NativeInt ) : PPyObject; virtual;
    function  SqAssItem( idx : NativeInt; obj : PPyObject) : Integer; virtual;
    function  SqAssSlice( idx1, idx2 : NativeInt; obj : PPyObject): integer; virtual;
    function  SqContains( obj: PPyObject): integer; virtual;
    function  SqInplaceConcat( obj : PPyObject): PPyObject; virtual;
    function  SqInplaceRepeat( i: NativeInt): PPyObject; virtual;
    // Mapping services
    function  MpLength : NativeInt; virtual;
    function  MpSubscript( obj : PPyObject) : PPyObject; virtual;
    function  MpAssSubscript( obj1, obj2 : PPyObject) : Integer; virtual;

    // Class methods
    class procedure RegisterMethods( APythonType : TPythonType ); virtual;
    class procedure RegisterMembers( APythonType : TPythonType ); virtual;
    class procedure RegisterGetSets( APythonType : TPythonType ); virtual;
    class procedure SetupType( APythonType : TPythonType ); virtual;
  end;
  TPyObjectClass = class of TPyObject;

  TBasicServices     = set of (bsPrint, bsGetAttr, bsSetAttr,
                               bsRepr, bsCompare, bsHash,
                               bsStr, bsGetAttrO, bsSetAttrO,
                               bsCall,
                               // since version 2.0
                               bsTraverse, bsClear,
                               // since version 2.1
                               bsRichCompare,
                               // since version 2.2
                               bsIter, bsIterNext);
  TNumberServices    = set of (nsAdd, nsSubstract, nsMultiply,
                               nsDivide, nsRemainder, nsDivmod,
                               nsPower, nsNegative, nsPositive,
                               nsAbsolute, nsNonZero, nsInvert,
                               nsLShift, nsRShift, nsAnd,
                               nsXor, nsOr, nsCoerce,
                               nsInt, nsLong, nsFloat,
                               nsOct, nsHex,
                               // since version 2.2
                               nsFloorDivide, nsTrueDivide);

  // TInplaceNumberServices exists since version 2.0
  TInplaceNumberServices = set of (nsInplaceAdd, nsInplaceSubtract,
                                   nsInplaceMultiply, nsInplaceDivide,
                                   nsInplaceRemainder, nsInplacePower,
                                   nsInplaceLShift, nsInplaceRShift,
                                   nsInplaceAnd, nsInplaceXor, nsInplaceOr,
                                   // since version 2.2
                                   nsInplaceFloorDivide, nsInplaceTrueDivide);

  TSequenceServices  = set of (ssLength, ssConcat, ssRepeat,
                               ssItem, ssSlice, ssAssItem,
                               ssAssSlice,
                               // since version 2.0
                               ssContains,
                               ssInplaceConcat,
                               ssInplaceRepeat
                               );

  TMappingServices   = set of (msLength, msSubscript, msAssSubscript);

  TTypeServices = class(TPersistent)
    protected
      FBasic          : TBasicServices;
      FNumber         : TNumberServices;
      FSequence       : TSequenceServices;
      FMapping        : TMappingServices;
      FInplaceNumber  : TInplaceNumberServices;

    public
      constructor Create;
      procedure AssignTo( Dest: TPersistent ); override;

    published
      property Basic : TBasicServices read FBasic write FBasic;
      property InplaceNumber : TInplaceNumberServices read FInplaceNumber Write FInplaceNumber;
      property Number : TNumberServices read FNumber write FNumber;
      property Sequence : TSequenceServices read FSequence write FSequence;
      property Mapping : TMappingServices read FMapping write FMapping;
  end;

  // The component that initializes the Python type and
  // that creates instances of itself.
  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonType = class(TGetSetContainer)
    protected
      FType : PyTypeObject;
      FTypeName : AnsiString;
      FModule : TPythonModule;
      FPyObjectClass : TPyObjectClass;
      FPrefix : AnsiString;
      FCreateFuncName : AnsiString;
      FServices : TTypeServices;
      FNumber:   PyNumberMethods;
      FSequence: PySequenceMethods;
      FMapping:  PyMappingMethods;
      FCurrentDocString: AnsiString;
      FDocString: TStringList;
      FCreateFuncDoc : AnsiString;
      FInstanceCount : Integer;
      FCreateHits : Integer;
      FDeleteHits : Integer;
      FTypeFlags : TPFlags;
      FCreateFunc : PPyObject;
      FCreateFuncDef : PyMethodDef;
      FGenerateCreateFunction: Boolean;

      procedure Notification( AComponent: TComponent;
                              Operation: TOperation); override;
      function  GetTypePtr : PPyTypeObject;
      procedure SetPyObjectClass( val : TPyObjectClass );
      procedure SetModule( val : TPythonModule );
      procedure SetServices( val : TTypeServices );
      procedure SetTypeName( const val : AnsiString );
      function  CreateMethod( pSelf, args : PPyObject ) : PPyObject; cdecl;
      procedure InitServices;
      procedure SetDocString( value : TStringList );
      function  TypeFlagsAsInt : LongInt;
      function  GetMembersStartOffset : Integer; override;
      procedure ModuleReady(Sender : TObject); override;
      procedure ReallocMethods; override;
      procedure ReallocMembers; override;
      procedure ReallocGetSets; override;

      // Type services
      // They will be all forwarded to the Delphi class that
      // implements the object through the use of virtual
      // methods
      ///////////////////////////////////////
      function  NewSubtypeInst( aType: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;

    public
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      procedure Initialize; override;
      procedure Finalize; override;
      function  CreateInstance : PPyObject;
      function  CreateInstanceWith( args : PPyObject ) : PPyObject;
      procedure AddTypeVar;

      property TheType : PyTypeObject read FType write FType;
      property TheTypePtr : PPyTypeObject read GetTypePtr;
      property PyObjectClass : TPyObjectClass read FPyObjectClass write SetPyObjectClass stored False;
      property InstanceCount : Integer read FInstanceCount;
      property CreateHits : Integer read FCreateHits;
      property DeleteHits : Integer read FDeleteHits;

    published
      property DocString : TStringList read FDocString write SetDocString;
      property TypeName : AnsiString read FTypeName write SetTypeName;
      property TypeFlags : TPFlags read FTypeFlags write FTypeFlags default TPFLAGS_DEFAULT;
      property Prefix : AnsiString read FPrefix write FPrefix;
      property Module : TPythonModule read FModule write SetModule;
      property Services : TTypeServices read FServices write SetServices;
      property GenerateCreateFunction : Boolean read fGenerateCreateFunction write fGenerateCreateFunction default True;
  end;

//-------------------------------------------------------
//--                                                   --
//--  class: TPythonVar derived from TEngineClient     --
//--                                                   --
//-------------------------------------------------------

  TGetDataEvent = procedure ( Sender : TObject; var Data : Variant ) of Object;
  TSetDataEvent = procedure ( Sender : TObject; Data : Variant ) of Object;
  TExtGetDataEvent = procedure ( Sender : TObject; var Data : PPyObject ) of Object;
  TExtSetDataEvent = procedure ( Sender : TObject; Data : PPyObject) of Object;

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonDelphiVar = class( TEngineClient )
    protected
      FModule    : AnsiString;
      FVarName   : AnsiString;
      FVarObject : PPyObject;
      FOnGetData : TGetDataEvent;
      FOnSetData : TSetDataEvent;
      FOnExtGetData : TExtGetDataEvent;
      FOnExtSetData : TExtSetDataEvent;
      FOnChange  : TNotifyEvent;

      procedure CreateVarType;
      procedure CreateVar;
      function  GetValue : Variant;
      procedure SetValue( const val : Variant );
      function  GetValueAsPyObject : PPyObject;
      procedure SetValueFromPyObject( val : PPyObject );
      function  GetValueAsString : string;
      procedure SetVarName( const val : AnsiString );

    public
      // Constructors & Destructors
      constructor Create( AOwner : TComponent ); override;

      // Public methods
      procedure Initialize; override;
      procedure Finalize; override;
      function  IsVariantOk( const v : Variant ) : Boolean;

      // Public properties
      property Value : Variant read GetValue write SetValue;
      // Warning: ValueObject returns a preincremented object !
      property ValueObject : PPyObject read GetValueAsPyObject write SetValueFromPyObject;
      property ValueAsString : string read GetValueAsString;
      property VarObject : PPyObject read FVarObject write FVarObject;

    published
      property Module    : AnsiString read FModule write FModule;
      property VarName   : AnsiString read FVarName write SetVarName;
      property OnGetData : TGetDataEvent read FOnGetData write FOnGetData;
      property OnSetData : TSetDataEvent read FOnSetData write FOnSetData;
      property OnExtGetData : TExtGetDataEvent read FOnExtGetData write FOnExtGetData;
      property OnExtSetData : TExtSetDataEvent read FOnExtSetData write FOnExtSetData;
      property OnChange  : TNotifyEvent read FOnChange write FOnChange;
  end;

  TPyVar = class(TPyObject)
  public
    dv_var         : Variant;
    dv_component   : TPythonDelphiVar;
    dv_object      : PPyObject;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;
    destructor  Destroy; override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( APythonType : TPythonType ); override;

    // Methods of TPyVar
    function GetValue : PPyObject;
    function GetValueAsVariant : Variant;
    procedure SetValue( value : PPyObject );
    procedure SetValueFromVariant( const value : Variant );

    // Interface methods
  end;

//#######################################################
//##                                                   ##
//##  Thread Object with Python interpreter lock       ##
//##                                                   ##
//#######################################################
  TThreadExecMode = (emNewState, emNewInterpreter);

{$HINTS OFF}
  TPythonThread = class(TThread)
  private
    f_savethreadstate: PPyThreadState;
    fInterpreterState: PPyInterpreterState;
    fThreadState:      PPyThreadState;
    fThreadExecMode:   TThreadExecMode;

// Do not overwrite Execute! Use ExecuteWithPython instead!
    procedure Execute; override;
  protected
    procedure ExecuteWithPython; virtual; abstract;

    procedure Py_Begin_Allow_Threads;
    procedure Py_End_Allow_Threads;
// The following procedures are redundant and only for
// compatibility to the C API documentation.
    procedure Py_Begin_Block_Threads;
    procedure Py_Begin_Unblock_Threads;

  public
    property InterpreterState: PPyInterpreterState read  fInterpreterState
                                                   write fInterpreterState
                                                   default nil;
    property ThreadState: PPyThreadState read  fThreadState
                                         write fThreadState;
    property ThreadExecMode: TThreadExecMode read fThreadExecMode;
  end;
{$HINTS ON}

//#######################################################
//##                                                   ##
//##        New Python objects                         ##
//##                                                   ##
//#######################################################

//#######################################################
//##                                                   ##
//##    Methods for new Python objects or modules      ##
//##                                                   ##
//#######################################################

// Module pyio for Python Input/Outputs
function  pyio_write(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_read(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_SetDelayWrites(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_SetMaxLines(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_GetTypesStats(self, args : PPyObject) : PPyObject; cdecl;


//#######################################################
//##                                                   ##
//##        Global procedures                          ##
//##                                                   ##
//#######################################################

function  GetPythonEngine : TPythonEngine;
function  PythonOK : Boolean;
function  PythonToDelphi( obj : PPyObject ) : TPyObject;
function  IsDelphiObject( obj : PPyObject ) : Boolean;
procedure PyObjectDestructor( pSelf : PPyObject); cdecl;
procedure FreeSubtypeInst(ob:PPyObject); cdecl;
procedure Register;
function  PyType_HasFeature(AType : PPyTypeObject; AFlag : Integer) : Boolean;

{ Helper functions}
(*
    Checks whether the PythonVersion x.x is Registered
*)
{$IFDEF MSWINDOWS}
function IsPythonVersionRegistered(PythonVersion : string;
  out InstallPath: string; out AllUserInstall: Boolean) : Boolean;
{$ENDIF}
(*
  Mask FPU Excptions - Useful for importing SciPy and other Python libs
  See http://bugs.python.org/issue9980 and
  http://stackoverflow.com/questions/3933851/
*)
procedure MaskFPUExceptions(ExceptionsMasked : boolean;
  MatchPythonPrecision : Boolean = True);

//#######################################################
//##                                                   ##
//##        Global variables                           ##
//##                                                   ##
//#######################################################


(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MethodCallback'      Copyright (c) 1998                 *)
(*                                                                        *)
(* Version: 0.0                        Dr. Dietmar Budelsky               *)
(* Sub-Version: 0.3                    dbudelsky@web.de                   *)
(*                                     Germany                            *)
(*                                                                        *)
(**************************************************************************)
(*  Functionality: Generates synthetic callback functions which calls     *)
(*  DELPHI Class Methods. A callback mechanism (DDE, PYTHON, TCL) can now *)
(*  use DELPHI objects.                                                   *)
(*                                                                        *)
(**************************************************************************)
(*  Contributors:                                                         *)
(*      Grzegorz Makarewicz (mak@mikroplan.com.pl)                        *)
(*      Morgan Martinet     (p4d@mmm-experts.com)                         *)
(*      Samuel Iseli        (iseli@vertec.ch)                             *)
(*      Andrey Gruzdev      (andrey.gruzdev@gmail.com)                    *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free, as long as this  *)
(* header and its copyright text is intact.                               *)
(* Dr. Dietmar Budelsky, 1998-01-07                                       *)
(**************************************************************************)

//{$I Definition.Inc}
//unit MethodCallBack;
//interface
//uses SysUtils;

type
  TCallType = (ctSTDCALL, ctCDECL);
  TCallBack = procedure of object;
  TDDEAPIfunc = function( CallType, Fmt: Integer; Conv: longint;
                          hsz1, hsz2: longint;
                          Data: longint; Data1, Data2: integer):
                          longint of object; stdcall;
// Method declaration for DDE(ML) interface. Callbackmethods for DDE(ML) have
// to be declared according to this.

function GetDDECallBack(method: TDDEAPIfunc): Pointer;
// Call for example with
// GetDDECallBack(DDECallBackMethod);

function  GetCallBack( self: TObject; method: Pointer;
                       argnum: Integer; calltype: tcalltype): Pointer;
// Call for example with
// CallBackProc := GetCallBack( self, @TSelfObject.Method, 2, ctSTDCALL);
//
// "self" is a valid TSelfObject,
// "Method" is a pointer to the class method, which should be triggered,
// when CallBackProc is called. It has to be declared according to the
// calltype!
// argnum is the number of callback parameters. There are the following
// exceptions: Double and Currency count for two. (sure)
//             Float counts for two               (not tested yet)
//             Extended counts for three          (not tested yet)
//             Records count for SizeOf(record)/4 rounded up.
// calltype is the calling convention of the callback function.

function  GetOfObjectCallBack( CallBack: TCallBack;
                               argnum: Integer; calltype: TCallType): Pointer;
// More sophisticated interface for standardized callback mechanisms.
// Usage for example:
// type
// TMyCallBack = function(x: Integer):Integer of object; cdecl;
// TMyClass = Class
//   CallBackProc: Pointer;
//   function y(x: Integer):Integer; cdecl;
//   procedure Init;
// end;
// ...
// function SetCallBack(f: TMyCallBack): Pointer;
// begin
//   result := GetOfObjectCallBack( TCallBack(f), 1, ctCDECL);
// end;
// procedure TMyClass.Init;
// begin
//   CallBackProc := SetCallBack(y);
// end;

procedure DeleteCallBack( Proc: Pointer );
// frees the memory used for Proc. Call with
// DeleteCallBack( CallBackProc);

function CodeMemPageCount: integer;
// returns the page count allocated for callbacks
// mainly for test purposes

procedure FreeCallBacks;
// frees all callbacks
// is called on finalize unit
// should only be called explicitely for testing

implementation
