
// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see
// <http://www.gnu.org/licenses/>.


#ifdef _USE_getlinknumbersbyname

// return a list of link numbers of prims with string as name unless empty
list getlinknumbersbyname(string name) {
	integer n = llGetObjectPrimCount(llGetKey());
	list numbers = [];

	name = llToLower(name);

	if(n > 1)
		{
			while(n) {
				if(Instr(llToLower(llGetLinkName(n)), name))
					{
						numbers += n;
					}
				--n;
			}
		}
	else
		{
			if(Instr(llToLower(llGetObjectName()), name) )
				{
					numbers += n;
				}
		}
	return numbers;
}

#endif


#ifdef _USE_getlinknumbersbyname_attached

// return a list of link numbers of prims with string as name unless empty
list getlinknumbersbyname_attached(string name)
{
	integer n = llGetNumberOfPrims();
	list numbers = [];

	name = llToLower(name);

	while(n >= 1)
		{
			if(Instr(llToLower(llGetLinkName(n)), name))
				{
					numbers += n;
				}
			--n;
		}
	if(Instr(llToLower(llGetObjectName()), name) && NotOnlst(numbers, n))
		{
			numbers += n;
		}
	return numbers;
}

#endif


#ifdef _USE_getassignedlinknumbersbyname

list getassignedlinknumbersbyname(string name, string desc)
{
	integer n = llGetObjectPrimCount(llGetKey());
	list numbers = [];

	name = llToLower(name);
	desc = llToLower(desc);

	if(n > 1) {
		while(n) {
			if(Instr(llToLower(llGetLinkName(n)), name) && Instr(llToLower(llList2String(GLPP(n, [PRIM_DESC] ), 0)), desc))
				{
					numbers += n;
				}
			--n;
		}
	}
	else {
		if(Instr(llToLower(llGetObjectName()), name) && Instr(llToLower(llGetObjectDesc()), desc))
			{
				numbers += n;
			}
	}
	return numbers;
}

#endif


#ifdef _USE_getsinglelinknumberbyname

// return the number of the link of the prim with string as name unless -1

int getsinglelinknumberbyname(string name)
{
	integer n = llGetObjectPrimCount(llGetLinkKey(llGetLinkNumber() )  );
	name = llToLower(name);
	if(n > 1)
		{
			do
				{
					if(Instr(llToLower(llGetLinkName(n)), name))
						{
							return n;
						}
					--n;
				}
			while(n);
		}
	else
		{
			if(Instr(llToLower(llGetObjectName()), name))
				{
					return llGetLinkNumber();
				}
		}
	return -1;
}

#endif


#ifdef _USE_getsinglelinknumberbyname_attached

// return the number of the link of the prim with string as name unless -1

int getsinglelinknumberbyname_attached(string name)
{
	integer n = llGetNumberOfPrims();
	name = llToLower(name);
	do
		{
			if(Instr(llToLower(llGetLinkName(n)), name))
				{
					return n;
				}
			--n;
		}
	while(n > 0);

	return -1;
}

#endif


#ifdef _USE_getlinknumbersbylistnamed_attached

// return a list of link numbers of prims that have a name on the list of names given and their names in a strided list

list getlinknumbersbylistnamed_attached(list names)
{
	//	int n = Len(names);
	//	LoopDown(n, names = llListReplaceList(names, llToLower(llList2String(names, n)), n, n));

        list strided = [];

	integer n = llGetNumberOfPrims();
	do
		{
                        string thisname = llGetLinkName(n);
			if(Onlst(names, thisname))
				{
                                        strided += [thisname, n];
				}
			--n;
		}
	while(n > 0);

	return strided;
}

#endif


#ifdef _USE_getlinknumbersbylistnamedappend_attached

// return a list of link numbers of prims that have a name on the list of names given and their names in a strided list

list getlinknumbersbylistnamedappend_attached(list names)
{
	//	int n = Len(names);
	//	LoopDown(n, names = llListReplaceList(names, llToLower(llList2String(names, n)), n, n));

        list strided = [];

	integer n = llGetNumberOfPrims();
	do
		{
                        string thisname = llGetLinkName(n);
			if(Onlst(names, thisname))
				{
					Enlist(strided, thisname, n, _GETLINKNUMBERSBYLISTNAMEDAPPEND_ATTACHED_APPENDIX);
				}
		}
	while(--n > 0);

	DEBUGmsgLIB(llList2CSV(strided));
	return strided;
}

#endif


#ifdef _USE_getlinknumbersbylistnamedappend_attached_notnamed

// return a list of link numbers of prims that have a name on the list of names given in a strided list
// this is identical with getlinknumbersbylistnamedappend_attached() except that the list returned does
// not contain the names of the prims
//
list getlinknumbersbylistnamedappend_attached_notnamed(list names)
{
	//	int n = Len(names);
	//	LoopDown(n, names = llListReplaceList(names, llToLower(llList2String(names, n)), n, n));

        list strided = [];

	integer n = llGetNumberOfPrims();
	do
		{
                        string thisname = llGetLinkName(n);
			if(Onlst(names, thisname))
				{
					Enlist(strided, n, _GETLINKNUMBERSBYLISTNAMEDAPPEND_ATTACHED_NOTNAMED_APPENDIX);
				}
		}
	while(--n > 0);

	return strided;
}

#endif


#ifdef _USE_getlinknumbersbylist

// return a list of link numbers of prims that have a name on the list of names given

list getlinknumbersbylist(list names)
{
	integer n = llGetObjectPrimCount(llGetKey());
	list l = [];

	if(n > 1)
		{
			do
				{
					if(Onlst(names, llToLower(llGetLinkName(n))))
						{
							l += n;
						}
					--n;
				}
			while(n);
		}
	else
		{
			if(Onlst(names, llToLower(llGetObjectName())))
				{
					l += n;
				}
		}
	return l;
}

#endif


#ifdef _USE_isstillsittingon

// return the link number k is sitting on unless -1

int isstillsittingon(key k)
{
	if(k)
		{
			int n = llGetNumberOfPrims();
			if(n > 1)
				{
					while(n)
						{
							if(llGetLinkKey(n) == k)
								{
									return n;
								}
							--n;
						}
				}
		}
	// if nobody sits, there's only one prim
	return -1;
}

#endif


#ifdef _USE_whoissitting

// return a list of avas sitting on the linkset unless empty
list whoissitting()
{
	list agents = [];
	int n = llGetNumberOfPrims();
	if(n > 1)
		{
			while(n)
				{
					key lk = llGetLinkKey(n);
					if(llGetAgentSize(lk) != ZERO_VECTOR)
						{
							agents += [lk];
						}
					--n;
				}
		}
	return agents;
}

#endif


#ifdef _USE_getlargestscale

// return the number of the link of the prim with string as name unless -1

float getlargestscale()
{
	integer n = llGetObjectPrimCount(llGetKey());
	vector largest = ZERO_VECTOR;
	if(n > 1)
		{
			do
				{
					vector this = llList2Vector(GLPP(n, [PRIM_SIZE]), 0);
					largest.x = FMax(largest.x, this.x);
					largest.y = FMax(largest.y, this.y);
					largest.z = FMax(largest.z, this.z);
					--n;
				}
			while(n > 0);
		}
	else
		{
			vector this = llList2Vector(GLPP(LINK_ROOT, [PRIM_SIZE]), 0);
			largest.x = FMax(largest.x, this.x);
			largest.y = FMax(largest.y, this.y);
			largest.z = FMax(largest.z, this.z);
		}
	return FMax(FMax(largest.x, largest.y), largest.z);
}

#endif
