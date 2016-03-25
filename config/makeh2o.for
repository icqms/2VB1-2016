c     **** prog to make the 72 test water orientations ****

      implicit real*8 (a-h,o-z)

      real*8 r(3,3),a(3),b(3),br(3),b1(3),c(3),c1(3),c2(3)
      data b1 / .3,.4,.5 /
      data c1 / -.3,.6,-.2 /

      ah= 0.97 * cos (104.5/2./180.*3.14159265)
      bh= 0.97 * sin (104.5/2./180.*3.14159265)

      open (1,file='icosa',status='old')
      read (1,*)
      read (1,*) n
      do icosa= 1,n
	read (1,*) a
	call norm (a)

c	**** GS orthogonalization ****
	call norm (b1)
	call gs (b,a,b1)
	call norm (c1)
	call gs (c2,a,c1)
	call gs (c,b,c2)

c	**** theta rotation ****
	do itheta= 0,150,30
	  theta= itheta/180. * 3.14159265
	  ct= cos (theta)
	  st= sin (theta)
	  br(1)= c(1)*st + b(1)*ct
	  br(2)= c(2)*st + b(2)*ct
	  br(3)= c(3)*st + b(3)*ct
	  write (6,800) (ah*a(i)+bh*br(i),i=1,3),
     $		        (ah*a(i)-bh*br(i),i=1,3)
	end do

      end do

800   format (6f10.6)
      end

      subroutine norm (x)

      implicit real*8 (a-h,o-z)
      real*8 x(3)

      xlen= sqrt (x(1)**2 + x(2)**2 + x(3)**2)
      x(1)= x(1) / xlen
      x(2)= x(2) / xlen
      x(3)= x(3) / xlen

      return
      end

      subroutine gs (c,a,b)

      implicit real*8 (a-h,o-z)
      real*8 a(3),b(3),c(3)

      dot= a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
      do j= 1,3
	c(j)= b(j) - dot * a(j)
      end do

      call norm (c)

      end
