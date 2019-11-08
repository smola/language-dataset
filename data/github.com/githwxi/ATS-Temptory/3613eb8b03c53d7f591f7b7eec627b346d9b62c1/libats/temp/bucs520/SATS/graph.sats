(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2011-2019 Hongwei Xi, ATS Trustful Software, Inc.
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* Author: Hongwei Xi *)
(* Start time: February, 2019 *)
(* Authoremail: hwxiATcsDOTbuDOTedu *)

(* ****** ****** *)
//
(*
HX-2019-05:
For
streamizng generic graphs
*)
//
(* ****** ****** *)
//
#define
ATS_PACKNAME
"temptory.temp.bucs520."
//
(* ****** ****** *)
//
fun
{node:vtflt}
graph_store_free(): void
//
(* ****** ****** *)
//
fun
{node:vtflt}
graph_store_insert
  (nx: node): void
fun
{node:vtflt}
graph_store_insert_list
  (nxs: list0_vt(node)): void
//
(* ****** ****** *)

fun
{node:vtflt}
graph_store_choose_opt
  ((*void*)): optn0_vt(node)

(* ****** ****** *)
//
fun
{node:vtflt}
graph_node_mark(!node): void
fun
{node:vtflt}
graph_node_unmark(!node): void
//
fun
{node:vtflt}
graph_node_is_marked(!node): bool
//
(* ****** ****** *)
//
fun
{node:vtflt}
graph_streamize(node): stream_vt(node)
//
(* ****** ****** *)
//
fun
{node:vtflt}
graph_streamize_dfs(node): stream_vt(node)
fun
{node:vtflt}
graph_streamize_bfs(node): stream_vt(node)
//
(* ****** ****** *)
//
fun
{node:vtflt}
graph_node_neighbors(nx0: !node): list0_vt(node)
//
(* ****** ****** *)

(* end of [cs520_graph.sats] *)
