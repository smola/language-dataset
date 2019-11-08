function [centers,U,ofun,ofunk,em]=fcmeans(Xin,c,m,maxiter,epsilon,verbose)
//Data clustering using fuzzy c-means.
//Calling Sequence
//[centers,U,ofun,ofunk,em]=fcmeans(Xin,c,m [,maxiter [,epsilon [,verbose]]])
//Parameters
// Xin:matrix of reals.The pairs of inputs points.
// c:integer, number of clusters.
// m:scalar, fizzifier constant.
// maxiter:integer, maximum number of iterations. The defaul value is 100
// epsilon:scalar, minimum change value between two consecutive iterations. The default value is 0.001
// verbose:boolean, display information.The default value is %f.
//Description
//         <literal>fcmeans </literal> find the <literal>c</literal> number of clusters in the
//     data set <literal>Xin</literal> using fuzzy c-means algorithm. The centers for
//     each cluster are returned in <literal>centers</literal>. <literal>U</literal> contains
//     the grade of membership of each <literal>Xin</literal> point in each cluster.
//     <literal>ofun</literal> is the last objetive function. <literal>ofunk</literal> is the
//     objetive function in each iteration. <literal>em</literal> is the exit mode, if
//     <literal>em</literal> is <literal>%t</literal> then the maximum number of iteration
//     <literal>maxiter</literal> was reached, if <literal>em</literal> is <literal>%f</literal>
//     then the minimum change between iteration <literal>epsilon</literal> was
//     reached.
//Examples
// // Take 50 random pairs of points 
//Xin=rand(100,2);
// // Find 7 clusters
// [centers,U,ofun,ofunk]=fcmeans(Xin,7,2);
// // Display information
// scf();clf();
// subplot(2,2,1);
// plot2d(Xin(:,1),Xin(:,2),-1,rect=[0 0 1 1]);
// xtitle("Input pair of points","x","y");
// subplot(2,2,3);
// plot2d(centers(:,1),centers(:,2),-2,rect=[0 0 1 1]);
// xtitle("Cluster centers","x","y");
// subplot(2,2,2);
// plot(ofunk);
// xtitle("Objetive function in each iteration","k","ofun");
//See also
//subclust
//inwichclust
// Authors
// Jaime Urzua Grez
// Holger Nahrstaedt


// ----------------------------------------------------------------------
// Fuzzy C-Means
// ----------------------------------------------------------------------
// This file is part of sciFLT ( Scilab Fuzzy Logic Toolbox )
// Copyright (C) @YEARS@ Jaime Urzua Grez
// mailto:jaime_urzua@yahoo.com
// 
// 2011 Holger Nahrstaedt
// ----------------------------------------------------------------------
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// ----------------------------------------------------------------------

// Check and get RHS
rhs=argn(2);
if (rhs<3) then
	error("fcmeans need at least 3 parameters.");
end

if (rhs<4) then
	maxiter=100; // Default number of iterations
end

if (rhs<5) then
	epsilon=0.001; // Default maximum difference between two consecutive steps
end

if (rhs<6) then
	verbose=%f; // No verbose mode
end

n=size(Xin,1); // Number of pairs of inputs
nd=size(Xin,2); // Dimension of pairs of inputs

if (m<=1) then
	error("The m parameter must be great than 1.");
end

if (c<2)|(c>=n) then
	error("The number ob clusters must be 1<c<(number_pair_of_points-1)");
end

// Initialize and normalize initial U
U=rand(n,c);
U=U ./ ( sum(U,"c").*.ones(1,c) );

// Initialize some internal values
niter=0; // Number of iterations
lofun=%inf; // Las objetive function
ofun=0; // Objetive function
goon=%t;
ofunk=[];

// Make the real work
while (niter<=maxiter)&(goon)
	// Compute the centers
	Um=(U').^m;
	centers=(Um*Xin) ./ ( sum(Um,"c").*.ones(1,nd) );
	//  Calculate the square distance and the objetive function
	sd=[];
	for k=1:c,
//		sd=[sd sum((Xin-centers(k,:).*.ones(n,1)).^2,"c")];
		sd=[sd sum((Xin-repvec(n,centers(k,:))).^2,"c")];
	end
	ofun=sum((Um').*sd);
	if (verbose & (niter>0) ) then
		write(%io(2),"Iteration = "+string(niter)+" ofun="+string(ofun));
	end
	
	if (abs(lofun-ofun)>epsilon) then
		ofunk=[ofunk;ofun];
		lofun=ofun
		sd=sd.^(1/(m-1));
		// Update the membership
		for j=1:c,
			s1=0;
			for k=1:c,
				s1=s1+sd(:,j)./sd(:,k);
			end
			U(:,j)=(1 ./ s1);
		end
		niter=niter+1;
	else
		goon=%f;
	end
end

// End mode
if (niter>maxiter) then
	em=%t;
else
	em=%f;
end

endfunction


