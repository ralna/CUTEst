C     COMMON SCALARS
      integer hnnz

C     COMMON ARRAYS
      integer hcol(hnnzmax),hlin(hnnzmax)
      real ( kind = rp_ ) hval(hnnzmax)

C     COMMON BLOCKS
      common /hdata/ hval,hlin,hcol,hnnz
      save   /hdata/
