      module newtonRoots
        implicit none
        contains

          function f(x,a,b,c)
            real :: x, f, a, b, c
!             f = a*x**2+b*x+c
             f = sin(x)
          end function
          
          function Df(x,a,b)
            real :: x, Df, a, b
!            Df = 2*a*x+b
            Df = cos(x)
          end function

          subroutine newton(a,b,c)
            integer :: i, MAXREPS=100
            real :: x_old, x, a, b, c
            x =-5
1           i = 1
            do
              if (abs(f(x,a,b,c)) < 1E-6) exit
              x = x_old - f(x,a,b,c)/Df(x,a,b)
              x_old = x
              i = i+1
              if (i > MAXREPS) then
                x = x+0.5
                goto 1
              end if
            end do
            print*, 'f(',x ,'):', f(x,a,b,c)
          end subroutine
      end module newtonRoots

      program main
        use newtonRoots
        call newton(1.,2.,-3.)
      end program main
