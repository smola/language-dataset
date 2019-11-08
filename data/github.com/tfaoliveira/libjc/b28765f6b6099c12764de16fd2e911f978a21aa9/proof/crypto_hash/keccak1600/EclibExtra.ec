(* Miscellaneous results on some constructions from the EC and Jasmin libraries *)
require import Core Int IntDiv List.
require BitEncoding.
(*---*) import IntExtra.

from Jasmin require JUtils.

lemma foldl_map ['a 'b 'c] (h:'a->'b) (f:'c ->'b->'c) (z:'c) l:
 foldl f z (List.map h l) = foldl (fun b a => f b (h a)) z l.
proof.
elim: l f z => //= x xs IH f z.
by rewrite IH.
qed.

lemma nth_inside ['a] d1 d2 (l: 'a list) i:
 0 <= i < size l =>
 nth d1 l i = nth d2 l i.
proof.
elim: l i => /=; first smt().
move=> x xs IH i Hi; case: (i=0) => E //.
by rewrite IH /#.
qed.

lemma nth0 (d:'a) l: nth d l 0 = head d l by case: l.

lemma take_take n1 n2 (l: 'a list):
 take n1 (take n2 l) = take (min n1 n2) l.
proof. elim: l n1 n2 => //= x xs IH n1 n2; smt(). qed.

lemma nth_nseq_dflt (d:'s) n i: nth d (nseq n d) i = d.
proof.
case: (0 <= i < n) => E; first by rewrite nth_nseq.
rewrite nth_out // size_nseq /#.
qed.

lemma nseq_add ['a] (x:'a) n1 n2:
 0 <= n1 => 0 <= n2 => nseq (n1+n2) x = nseq n1 x ++ nseq n2 x.
proof.
move=> Hn1 Hn2; elim/natind: n1 Hn1.
 by move=> n ??; rewrite (: n=0) 1:/# nseq0.
by move=> n Hn IH H; rewrite nseqS // (addzC n) -addzA addzC nseqS 1:/# IH //.
qed.

lemma take_nseq ['a] n1 n2 (x: 'a):
 take n1 (nseq n2 x) = nseq (min n1 n2) x.
proof.
elim/natind: n2 n1.
 move=> n Hn n1; rewrite !nseq0_le //.
 by have [? ?]:= min_is_lb n1 n; apply (lez_trans n).
move=> n Hn IH n1; case: (n1 <= 0) => /=.
 move=> ?; rewrite take_le0 // nseq0_le //.
 by have [? ?]:= min_is_lb n1 (n+1); apply (lez_trans n1).
rewrite -ltzNge=> ?; rewrite nseqS // take_cons H /=.
have ->: min n1 (n + 1) = (min (n1-1) n)+1.
 rewrite /min; case: (n1 < n+1) => ?.
  by have ->/=: n1 - 1 < n by smt().
 by have ->/=: !(n1 - 1 < n) by smt().
by rewrite nseqS /min /#.
qed.

lemma drop_nseq ['a] n1 n2 (x: 'a):
 0 <= n1 => drop n1 (nseq n2 x) = nseq (n2-n1) x.
proof.
elim/natind: n1 n2.
 move=> n Hn n2 Hn'; have ->: n=0 by smt().
 by rewrite drop0.
move=> n Hn IH n2 Hn'.
case: (n2 <= 0) => /= ?.
 by rewrite !nseq0_le // /#.
have ->: n2 = n2 - 1 + 1 by smt().
by rewrite nseqS 1:/# drop_cons ltzS Hn /= IH //; congr; smt().
qed.

lemma dropS ['a] (l: 'a list) n:
 0 <= n =>
 drop (n + 1) l = behead (drop n l).
proof.
elim: l n => [|x xs] //= IH n Hn.
have ->/=: !(n+1 <= 0) by smt().
case: (n=0).
 by move=> -> /=; rewrite drop0.
move=> ?; have ->/=: !(n<=0) by smt().
by rewrite -IH /#.
qed.

lemma drop_drop ['a] (l: 'a list) n1 n2:
 0 <= n1 => 0 <= n2 =>
 drop n1 (drop n2 l) = drop (n1+n2) l.
proof.
elim/natind: n1.
 by move=> n Hn; rewrite drop_le0 /#.
move=> n Hn IH H1 H2.
by rewrite dropS // IH // -dropS /#.
qed.

(* a variant of [size_take] that is more convenient in some cases *)
lemma size_take' ['a] n (s: 'a list):
 0 <= n => size (take n s) = if n <= size s then n else size s.
proof.
move=> Hn; rewrite size_take //.
case: (n = size s) => E; first by rewrite E.
case: (n <= size s) => H.
 by rewrite (: n < size s) /#.
by rewrite (: ! n < size s) /#.
qed.

(* likewise for [take_cat] *)
lemma take_cat' ['a] n (s1 s2: 'a list):
 take n (s1++s2) = if n <= size s1 then take n s1 else s1 ++ take (n-size s1) s2.
proof.
rewrite take_cat //.
case: (n = size s1) => E.
 by rewrite !E /= take0 cats0 take_size.
case: (n <= size s1) => H.
 by rewrite (: n < size s1) /#.
by rewrite (: ! n < size s1) /#.
qed.

(* [eq_mkseq'] is a more refined version of [eq_mkseq] *)
lemma eq_mkseq' ['a] (f g : int -> 'a) n:
  (forall (x : int), 0 <= x < n => f x = g x) => mkseq f n = mkseq g n.
proof.
elim/natind: n f g => /=.
 by move=> n Hn f g H; rewrite !mkseq0_le.
move=> n Hn IH f g H.
rewrite !(addzC n) !mkseq_add //; congr.
 by rewrite !mkseq1 H /#.
apply IH => x Hx /=.
by rewrite H /#.
qed.

lemma size_behead ['a] (l:'a list):
 size (behead l) = max 0 (size l - 1).
proof. by case: l => [|x xs] /=; rewrite /max //=; smt(size_ge0). qed.

lemma behead_map ['a 'b] (f:'a->'b) (l:'a list):
 behead (map f l) = map f (behead l)
by elim: l.

lemma behead_mkseq ['a] (f : int -> 'a) n:
 behead (mkseq f n) = mkseq (fun i=> f (1+i)) (n-1).
proof.
elim/natind: n => /=.
 by move=> n Hn; rewrite !mkseq0_le /#.
by move=> n Hn IH; rewrite addzC mkseq_add // mkseq1.
qed.

lemma take_mkseq ['a] n (f: int -> 'a) k:
 0 <= n => take n (mkseq f k) = mkseq f (min n k).
proof.
elim/natind: n => //=.
 move=> n Hn1 Hn2; have ->: n=0 by smt().
 by rewrite take0 mkseq0_le /#.
move => n Hn IH H.
case: (n < k) => E.
 rewrite min_lel 1:/#.
 rewrite (take_nth (f n)); first by rewrite size_mkseq /#.
 rewrite IH // mkseqS // min_lel 1:/#; congr.
 by rewrite nth_mkseq.
by rewrite min_ler 1:/# take_oversize // size_mkseq /#.
qed.

lemma drop_mkseq ['a] n (f: int -> 'a) k:
 0 <= n <= k => drop n (mkseq f k) = mkseq (fun i => f (i+n)) (k-n).
proof.
elim/natind: n => //=.
 move=> n Hn1 Hn2; have ->: n=0 by smt().
 by rewrite drop0.
move => n Hn IH H.
rewrite dropS // IH 1:/# behead_mkseq /= Ring.IntID.opprD addzA.
by apply eq_mkseq => x /=; congr; ring.
qed.

lemma behead_chunk ['a] n (l:'a list):
 behead (BitEncoding.BitChunking.chunk n l)
 = BitEncoding.BitChunking.chunk n (drop n l).
proof.
case: (size l < n).
 move=> ?; rewrite drop_oversize 1:/#.
 rewrite /BitEncoding.BitChunking.chunk behead_mkseq.
 rewrite divz_small /=; first smt(size_ge0).
 by rewrite mkseq0_le //= mkseq0.
case: (0 < n); last first.
rewrite -!lezNgt => ??.
 by rewrite drop_le0 // !BitEncoding.BitChunking.chunk_le0.
rewrite -lezNgt => ??.
rewrite /BitEncoding.BitChunking.chunk behead_mkseq /=.
rewrite size_drop // 1:/# max_ler 1:/#.
have ->: (size l - n) %/ n = size l %/ n - 1.
 have ->: size l = (size l - n) + 1*n by ring.
 by rewrite divzMDr /#.
by apply eq_mkseq' => x Hx /=; rewrite drop_drop /#.
qed.

lemma drop_chunk n k (l: 'a list):
 0 < k =>
 drop n (BitEncoding.BitChunking.chunk k l)
 = BitEncoding.BitChunking.chunk k (drop (k*n) l).
proof.
move=> Hk; elim/natind: n l.
 by move=> n Hn l; rewrite !drop_le0 /#.
move=> n Hn IH l; rewrite dropS // IH behead_chunk drop_drop 1,2:/#.
by congr; congr; ring.
qed.

lemma chunk_take_eq ['a] n (l:'a list):
 0 < n =>
 BitEncoding.BitChunking.chunk n l
 = BitEncoding.BitChunking.chunk n (take (size l %/ n * n) l).
proof.
move=> Hn; rewrite /BitEncoding.BitChunking.chunk.
have ->: (size (take (size l %/ n * n) l) %/ n) = (size l %/ n).
 rewrite size_take'; first smt(size_ge0).
 by rewrite lez_floor 1:/# /= mulzK 1:/#.
apply eq_mkseq' => x Hx /=.
rewrite -{1}(cat_take_drop (size l %/ n * n)).
rewrite drop_cat size_take'; first smt(size_ge0).
rewrite lez_floor 1:/# /= mulzC StdOrder.IntOrder.ltr_pmul2r 1:/#.
move: (Hx); move=> [? ->] /=.
have E: n <= size (drop (x * n) (take (size l %/ n * n) l)).
 rewrite size_drop 1:/# max_ler; last first.
  rewrite size_take' 1:/# lez_floor 1:/# /= -Ring.IntID.mulrBl.
  by rewrite -{1}mulz1 {1}mulzC StdOrder.IntOrder.ler_pmul2r // -ltzS /#.
 rewrite size_take' 1:/# lez_floor 1:/# /=.
 smt(size_ge0).
by rewrite take_cat' E.
qed.

op chunkfillsize (n sz:int) = (-sz)%%n.

lemma chunkfillsize_cmp n sz:
 0 < n => 0 <= chunkfillsize n sz < n.
proof. move=> Hn; rewrite /chunkfillsize; smt(JUtils.modz_cmp). qed.

lemma chunkfillsizeP n sz k:
 0 < n => chunkfillsize n (n*k+sz) = chunkfillsize n sz.
proof.
move=> Hn; rewrite /chunkfillsize.
by rewrite -modzNm mulzC modzMDl modzNm.
qed.

lemma chunkfillsizeE' n sz:
 0 < n => 0 < sz => chunkfillsize n sz = n - 1 - (sz-1) %% n
by move=> ??; rewrite /chunkfillsize modNz.

lemma divz_minus1 m d:
 0 < d => 0 <= m => ! d %| m => (m-1) %/ d = m %/ d.
proof.
move=> Hd Hm Hdvd.
rewrite {1}(divz_eq m d) -addzA divzMDl 1:/#; ring.
rewrite divz_small //; apply JUtils.bound_abs; split.
 by move: Hdvd; rewrite dvdzE; smt(JUtils.modz_cmp).
by move=> ?; smt(JUtils.modz_cmp).
qed.

lemma chunkfillsizeE n sz:
 0 < n => 0 <= sz => chunkfillsize n sz = if n %| sz then 0 else n - sz%%n.
proof.
move=> Hn Hsz'.
case: (n %| sz) .
 rewrite /chunkfillsize dvdzE -modzNm => ->; smt().
move=> ?.
have Hsz : 0 < sz by smt(dvdz0).
rewrite chunkfillsizeE' //. 
have ->: n - 1 - (sz - 1) %% n = n - (1 + (sz - 1) %% n) by ring.
congr; congr.
by rewrite !modzE divz_minus1 //; ring.
qed.

op chunkfill ['a] (d:'a) n l = l ++ nseq (chunkfillsize n (size l)) d.

lemma chunkfill_nil ['a] (d:'a) n:
 chunkfill d n [] = [].
proof. by rewrite /chunkfill /chunkfillsize. qed.

hint simplify chunkfill_nil.

lemma dvd_chunkfill ['a] (d:'a) n l:
 0 < n => n %| size l => chunkfill d n l = l.
proof.
by move=> Hn Hsz; rewrite /chunkfill chunkfillsizeE // ?size_ge0 !Hsz /= cats0.
qed.

lemma size_chunkfill ['a] (d:'a) n l:
 0 < n =>
 size (chunkfill d n l) = (size l - 1) %/ n * n + n.
proof.
move=> Hn; rewrite /chunkfill size_cat size_nseq max_ler //.
 smt(chunkfillsize_cmp).
case: (size l = 0) => E.
 by rewrite !E /= /chunkfillsize /= divNz //=; ring.
rewrite chunkfillsizeE' //; first smt(size_ge0).
have ->: size l + (n - 1 - (size l - 1) %% n) = (size l-1) + n - (size l-1) %% n
 by ring.
rewrite {1}(divz_eq (size l - 1) n); ring.
qed.

lemma chunkfillP ['a] (d:'a) n l:
 0 < n =>
 n %| size (chunkfill d n l).
proof.
move=> Hn; rewrite size_chunkfill //.
rewrite (: (size l - 1) %/ n * n + n = ((size l - 1) %/ n + 1) * n) 1:/#.
by rewrite dvdzE modzMl.
qed.

lemma chunkfillK ['a] (d:'a) n l:
 0 < n =>
 chunkfill d n (chunkfill d n l) = chunkfill d n l.
proof.
move=> Hn; rewrite {1}/chunkfill chunkfillsizeE // ?size_ge0.
by rewrite !chunkfillP //= cats0.
qed.

lemma chunkfill_cat (d:'a) n l1 l2:
 0 < n => n %| size l1 => chunkfill d n (l1++l2) = l1 ++ chunkfill d n l2.
proof.
move=> H0 Hsz; rewrite /chunkfill -!catA; congr; congr; congr.
move: Hsz; rewrite size_cat dvdzP => [[k]].
rewrite mulzC => ->.
by rewrite chunkfillsizeP.
qed.

lemma chunkfillK' l (d:'a) n:
 0 < n => n %| size l => chunkfill d n l = l.
proof.
rewrite /chunkfill => Hn Hsz.
move: (Hsz); rewrite dvdzP => [[k E]].
rewrite chunkfillsizeE //; first smt(size_ge0).
by rewrite !Hsz /= cats0.
qed.

lemma chunkfillsize_ge0 k n: 0 < k => 0 <= chunkfillsize k n.
proof.  by rewrite /chunkfillsize /#. qed.

lemma drop_chunkfill n (d:'a) k l:
 0 < k => 0 <= n =>
 drop (k*n) (chunkfill d k l) = chunkfill d k (drop (k*n) (chunkfill d k l)).
proof.
move=> Hk Hn; have := chunkfillP d k l Hk.
rewrite /chunkfill size_cat size_nseq max_ler; first smt(chunkfillsize_ge0).
move=> Hdvd; pose S:= chunkfillsize _ (size (drop _ _)).
have ->: S = 0; last by rewrite nseq0 cats0.
rewrite /S -(chunkfillsizeP _ _ n) // size_drop 1:/# addzC /max size_cat size_nseq. 
rewrite max_ler; first by apply chunkfillsize_ge0.
case: (0 < size l + chunkfillsize k (size l) - k * n) => E.
 rewrite Ring.IntID.subrK chunkfillsizeE; first 2 smt(size_ge0 chunkfillsize_ge0).
 by rewrite !Hdvd.
by rewrite addzC chunkfillsizeP.
qed.

lemma nth_chunkfill k (d:'a) n l:
 0 < n => nth d (chunkfill d n l) k = nth d l k.
proof.
move=> Hn; rewrite /chunkfill nth_cat.
case: (k < size l) => E //.
case: (0 <= k - size l < chunkfillsize n (size l)) => ?.
 by rewrite nth_nseq // nth_out /#.
rewrite nth_out.
 by rewrite size_nseq max_ler // chunkfillsize_ge0. 
by rewrite nth_out /#.
qed.

lemma size_chunkfilled (d:'a) k l:
 0 < k =>
 size (BitEncoding.BitChunking.chunk k (chunkfill d k l))
 = (size l - 1) %/ k + 1.
proof.
move=> Hk; rewrite BitEncoding.BitChunking.size_chunk //.
by rewrite size_chunkfill // divzMDl 1:/# divzz (:k<>0) 1:/#.
qed.

lemma head_chunkfilled (d:'a) k l:
 head [] (BitEncoding.BitChunking.chunk k (chunkfill d k l))
 = take k (chunkfill d k l).
proof.
case: (0 < k) => Hk; last first.
 by rewrite take_le0 1:/# BitEncoding.BitChunking.chunk_le0 1:/#.
case: (0 < size l) => Hsz; last first.
 have ->: l=[] by smt(size_ge0).
 by rewrite chunkfill_nil /= /BitEncoding.BitChunking.chunk /= mkseq0.
rewrite -nth0 /BitEncoding.BitChunking.chunk nth_mkseq /=.
 by rewrite size_chunkfill // /#.
by rewrite drop0.
qed.

lemma nth_chunkfilled (d:'a) k l (i:int):
 0 < k =>
 0 <= i < (size l - 1) %/ k + 1 =>
 nth [] (BitEncoding.BitChunking.chunk k (chunkfill d k l)) i
 = take k (drop (k*i) (chunkfill d k l)).
proof.
move=> Hk Hi.
rewrite {1}(:i = i+0) // -nth_drop 1,2:/# drop_chunk // nth0.
by rewrite drop_chunkfill // 1:/# head_chunkfilled.
qed.

lemma nth_chunkfilled' dl (d:'a) k l (i:int):
 0 < k =>
 0 <= i < (size l - 1) %/ k + 1 =>
 nth dl (BitEncoding.BitChunking.chunk k (chunkfill d k l)) i
 = mkseq (fun j => nth d l (k*i+j)) k.
proof.
move=> Hk Hi.
rewrite -(nth_inside []); first by rewrite size_chunkfilled.
rewrite (nth_chunkfilled d k l i) //.
have Hsz: size (take k (drop (k * i) (chunkfill d k l))) = k.
 rewrite size_take' 1:/# size_drop 1:/# size_chunkfill //.
 rewrite max_ler; first smt(size_ge0).
 have: k <= (size l - 1) %/ k * k + k - k * i.
  by move: Hi => [?]; rewrite ltzE => [?] /#.
 smt().
apply (eq_from_nth d); first by rewrite Hsz size_mkseq /#.
rewrite Hsz => j Hj.
rewrite nth_take 1,2:/# nth_drop 1,2:/# nth_chunkfill 1:/#.
by rewrite nth_mkseq.
qed.

lemma take_map2 ['a 'b 'c] (f:'a -> 'b -> 'c) n l1 l2:
 take n (JUtils.map2 f l1 l2) = JUtils.map2 f (take n l1) (take n l2).
proof.
elim: l1 l2 n => [|x xs IH] [|y ys] n //=; case:(n<=0) => // E.
by rewrite IH.
qed.

lemma drop_map2 ['a 'b 'c] (f:'a -> 'b -> 'c) n l1 l2:
 drop n (JUtils.map2 f l1 l2) = JUtils.map2 f (drop n l1) (drop n l2).
proof.
elim: l1 l2 n => [|x xs IH] [|y ys] n //=; case:(n<=0) => // E.
  by case: (drop (n - 1) ys) => [|_ _]. 
 by case: (drop (n - 1) xs) => [|_ _]. 
by rewrite IH.
qed.

lemma map2_nseq0r ['a 'b] (f:'a -> 'b -> 'a) x0 n l:
 size l = n =>
 (forall y, f y x0 = y) =>
 JUtils.map2 f l (nseq n x0) = l.
proof.
elim/natind: n l.
 move=> n Hn l Hl H.
 have E: n=0 by smt(size_ge0).
 by move: Hl; rewrite E size_eq0 nseq0 => ->.
move=> n Hn IH [|x xs] //=; first smt().
by move=> Hsz H; rewrite nseqS //= H IH /#.
qed.

