proc merge(l:int, mid:int, r:int){
    var lft:int = mid-l+1;
    var rht:int = r-mid;
    var rht__ar:[1..lft] int;
    var lft__ar:[1..rht] int;
    var j:int;
    j=1;
    for i in l..mid{
        rht__ar(j)=Arr(i);
        j=j+1;
    }
    j=1;
    for i in (mid+1)..r{
        lft__ar(j)=Arr(i);
        j=j+1;
    } 
    var i_l:int=1;
    var i_r:int=1;
    j=l;
    while(i_l<=lft && i_r<=rht){
        if(rht__ar(i_l)<=lft__ar(i_r)){
            Arr(j)=rht__ar(i_l);
            j=j+1;
            i_l=i_l+1;
        }
        else{
            Arr(j)=lft__ar(i_r);
            j=j+1;
            i_r=i_r+1;
        }
    }
    while(i_l<=lft){
        Arr(j)=rht__ar(i_l);
        i_l=i_l+1;
        j=j+1;  
    }
    while(i_r<=rht){
        Arr(j)=lft__ar(i_r);
        j=j+1;
        i_r=i_r+1;
    }
}

proc mergeSort(l:int, r:int){
    if(l<r){
        var mid:int = (l+r)>>1;
        mergeSort(l, mid);
        mergeSort(mid+1, r);
        merge(l, mid, r);
    }
}
