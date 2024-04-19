subroutine delaz(elat,elon,slat,slon,dx,dy,delta)
      avlat=0.5*(elat+slat)
      a=1.840708+avlat*(.0015269+avlat*(-.00034+avlat*(1.02337e-6)))
      b=1.843404+avlat*(-6.93799e-5+avlat*(8.79993e-6+avlat*(-6.47527e-8)))
      dlat=slat-elat
      dlon=slon-elon
      dx=a*dlon*60.
      dy=b*dlat*60.
      delta=sqrt(dx*dx+dy*dy)
End subroutine delaz
