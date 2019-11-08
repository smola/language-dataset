Bool BlkRead2(CDrv *dv,U8 *buf, I64 blk, I64 cnt)
{//Read blk cnt from Drv to buf.
  Bool res=TRUE,unlock;
  CBlkDev *bd=dv->bd;
  if (cnt<=0) return TRUE;
  DrvChk(dv);
  try {
    unlock=DrvLock(dv);
    CallExtStr("BlkDevInit",bd);
    if (dv->drv_offset && blk<dv->drv_offset ||
	  blk+cnt>dv->drv_offset+dv->size)
      throw('Drv');
    if (bd->flags & BDF_READ_CACHE)
      CallExtStr("RCache",dv,&buf,&blk,&cnt);
    if (cnt>0) {
      switch (bd->type) {
	case BDT_RAM:
	  MemCpy(buf,bd->RAM_dsk+blk<<BLK_SIZE_BITS,cnt<<BLK_SIZE_BITS);
	  break;
	case BDT_ISO_FILE_READ:
	case BDT_ISO_FILE_WRITE:
	  FBlkRead(bd->file_dsk,buf,blk,cnt);
	  break;
	case BDT_ATA:
	case BDT_ATAPI:
	  res=CallExtStr("ATARBlks",dv,buf,blk,cnt);
	  break;
	case BDT_VIRTIO_BLK:
		res=VIORBlks(dv,buf,blk,cnt);
		break;
      }
      bd->last_time=tS;
      if (bd->flags & BDF_READ_CACHE)
	CallExtStr("DskCacheAdd",dv,buf,blk,cnt);
    }
    if (unlock)
      DrvUnlock(dv);
  } catch
    if (unlock)
      DrvUnlock(dv);
  return res;
}

Bool BlkWrite2(CDrv *dv,U8 *buf, I64 blk, I64 cnt)
{//Write blk cnt from buf to Drv.
  Bool res=TRUE,unlock;
  CBlkDev *bd=dv->bd;
  if (cnt<=0) return TRUE;
  DrvChk(dv);
  try {
    unlock=DrvLock(dv);
    CallExtStr("BlkDevInit",bd);
    if (bd->flags&BDF_READ_ONLY && !(bd->flags & BDF_READ_ONLY_OVERRIDE))
      throw('BlkDev');
    if (dv->drv_offset && blk<dv->drv_offset ||
	  blk+cnt>dv->drv_offset+dv->size)
      throw('Drv');
    if (cnt>0) {
      switch (bd->type) {
	case BDT_RAM:
	  MemCpy(bd->RAM_dsk+blk<<BLK_SIZE_BITS,buf,cnt<<BLK_SIZE_BITS);
	  break;
	case BDT_ISO_FILE_READ:
	case BDT_ISO_FILE_WRITE:
	  FBlkWrite(bd->file_dsk,buf,blk,cnt);
	  break;
	case BDT_ATA:
	case BDT_ATAPI:
	  res=CallExtStr("ATAWBlks",dv,buf,blk,cnt);
	  break;
	case BDT_VIRTIO_BLK:
		res=VIOWBlks(dv,buf,blk,cnt);
		break;
      }
      bd->last_time=tS;
      if (bd->flags & BDF_READ_CACHE)
	CallExtStr("DskCacheAdd",dv,buf,blk,cnt);
    }
    if (unlock)
      DrvUnlock(dv);
  } catch
    if (unlock)
      DrvUnlock(dv);
  return res;
}

PatchJmpRel32(&BlkRead, &BlkRead2);
PatchJmpRel32(&BlkWrite, &BlkWrite2);
