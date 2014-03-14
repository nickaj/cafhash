! Copyright 2014 The University of Edinburgh

! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at

!     http://www.apache.org/licenses/LICENSE-2.0

! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.


module initialise
  use iso_fortran_env
  implicit none
  integer :: hashlen, collisions,localhashlen
  integer :: numi, inum
  integer, allocatable :: hashtab(:)[:],hashcount(:)[:]
  type(lock_type) :: iLock[*]

contains

 subroutine inithash()

   numi=num_images()
   inum=this_image()
   hashlen=2*NHASH*numi+1
   localhashlen=2*NHASH + 1
   collisions=0
   allocate(hashtab(localhashlen)[*],hashcount(localhashlen)[*])
   sync all

 end subroutine inithash

 subroutine finhash()
   deallocate(hashtab,hashcount)
 end subroutine finhash

 subroutine hashlookup(o,v)
   integer :: o,v
   integer :: desti, destpos,localCount,localHash,lv
   
   lv=v

   do while (1.gt.0)
      ! determine whom the hash belongs too
      desti = v /localhashlen
      if(desti*localhashlen.lt.v) then
         desti=desti+1
      end if
      
      destpos = v - (desti - 1)*localhashlen
      ! lock the data
      lock(iLock[desti])
      localhash=hashtab(destpos)[desti]
      ! get the hash
      if(localhash.eq.0) then
        ! insert the entry
         hashtab(destpos)[desti]=o
         hashcount(destpos)[desti]=1
         ! unlock before return
         unlock(ilock[desti])
         return
      else
         ! check to see if it is a collision
         if(localhash.eq.o) then
            ! its a repetition
            hashcount(destpos)[desti]=hashcount(destpos)[desti]+1
            unlock(iLock[desti])
            return
         else
            ! its a collision
            collisions=collisions+1
            v=v+1
            if(v.gt.hashlen) then
               v=1
            end if
            ! unlock before going round the loop
            unlock(iLock[desti])
        end if
      end if


   end do
 end subroutine hashlookup

end module initialise


program DHT
  use initialise
  integer :: pb,b,v,i,j

  call inithash()

  do j=1,2
     call fresetvalue(numi,inum)
     do i=1,NHASH
        call fnew(pb,b,v,numi)
        call hashlookup(b,v)
     end do
  end do
  
  sync all
  if(inum.eq.1) then
     
     do j=1,numi
        do i=1,localhashlen
           if(hashcount(i)[j].gt.0) then
              write(*,*) j,i,hashtab(i)[j],hashcount(i)[j]
           end if
        end do
     end do
  end if

  call finhash()

end program DHT


