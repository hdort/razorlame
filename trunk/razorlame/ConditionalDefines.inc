(* (c) Copyright 2001 - Holger Dors
   =======================================

   This file is part of RazorLame,
   a front-end for the LAME MP3 Encoder.

   See license.txt for details.             *)

//-- Common Options -------------------------------------------------
{$A+,B-,G+,H+,I+,J+,P+,T-,U-,V+,W-,X+,Z1}

//-- Beta & Debug Switches  ---------------------------------------------------

{$DEFINE BETAVER}         // Beta reminder at start up [changeable]
{.$DEFINE DEBUG}           // basic debug information [changeable]
{.$DEFINE DETAIL_DEBUG}
{$IFNDEF DEBUG}
  {$UNDEF DETAIL_DEBUG}
{$ENDIF}

{$IFDEF DEBUG}
  {$D+,C+,L+,O-,Q+,R+,Y+}
  {$HINTS ON}
  {$WARNINGS ON}
{$ELSE}
  {$D-,C-,L-,O+,Q-,R-,Y-}
{$ENDIF}
