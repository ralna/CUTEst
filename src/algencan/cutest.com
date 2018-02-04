C     COMMON SCALARS
      logical useslacks
      integer nranges,mcutest,ncutest

C     COMMON ARRAYS
      integer ccor(mmax),cmap(mmax),slaind(mmax)
      double precision ca(mmax),cb(mmax)

C     COMMON BLOCKS
      common /probdata/ ca,cb,ccor,cmap,slaind,mcutest,ncutest,
     +                  nranges,useslacks
      save   /probdata/

