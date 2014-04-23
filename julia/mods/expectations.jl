module expectations
export expect_y
# Generated tensor:
# expect_y V[a,y1,p1,z,t] * Gy[y,y1] * Gp[p,p1] | a,y,p,z,t
function expect_y(Gp,Gy,V)
np = size(V)[3]
na = size(V)[1]
nz = size(V)[4]
nt = size(V)[5]
ny = size(V)[2]
@inbounds begin
Res = zeros(na,ny,np,nz,nt)
for a in 1:na
 for y in 1:ny
  for p in 1:np
   for z in 1:nz
    for t in 1:nt
     Res[a,y,p,z,t]= 0
     for y1 in 1:ny
      for p1 in 1:np
       Res[a,y,p,z,t] =  Res[a,y,p,z,t].+V[a + na * (y1 + ny * (p1 + np * (z + nz * (t-1)-1)-1)-1)].*Gy[y + ny * (y1-1)].*Gp[p + np * (p1-1)]
      end
     end
    end
   end
  end
 end
end
end
return Res
end

end
