module E_tensors
export T_Evbar,T_FinalV,T_Final
# Generated tensor:
# T_Evbar V[a,h,y1,p1,Y1,P1,z1,tau,j] * Gz[z,z1] * Gp[p,p1,j] * Gy[y,y1,j] * GP[P,P1] * GY[Y,Y1] | a,h,y,p,Y,P,z,tau,j
function T_Evbar(GP,GY,Gp,Gy,Gz,V)
ntau = size(V)[8]
nh = size(V)[2]
nj = size(V)[9]
nY = size(V)[5]
na = size(V)[1]
ny = size(V)[3]
nz = size(V)[7]
np = size(V)[4]
nP = size(V)[6]
@inbounds begin
Res = zeros(na,nh,ny,np,nY,nP,nz,ntau,nj)
for a = 1:na
 for h = 1:nh
  for y = 1:ny
   for p = 1:np
    for Y = 1:nY
     for P = 1:nP
      for z = 1:nz
       for tau = 1:ntau
        for j = 1:nj
         Res[a,h,y,p,Y,P,z,tau,j]= 0
         for y1 = 1:ny
          for Y1 = 1:nY
           for P1 = 1:nP
            for p1 = 1:np
             for z1 = 1:nz
              Res[a,h,y,p,Y,P,z,tau,j] =  Res[a,h,y,p,Y,P,z,tau,j].+V[a + na * (h + nh * (y1 + ny * (p1 + np * (Y1 + nY * (P1 + nP * (z1 + nz * (tau + ntau * (j-1)-1)-1)-1)-1)-1)-1)-1)].*Gz[z + nz * (z1-1)].*Gp[p + np * (p1 + np * (j-1)-1)].*Gy[y + ny * (y1 + ny * (j-1)-1)].*GP[P + nP * (P1-1)].*GY[Y + nY * (Y1-1)]
             end
            end
           end
          end
         end
        end
       end
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

# Generated tensor:
# T_FinalV log(A_[a] + (H_[h] * P_[P,p,j] ) ) | a,h,P,j,p
function T_FinalV(A_,H_,P_)
nh = size(H_)[1]
na = size(A_)[1]
nj = size(P_)[3]
np = size(P_)[2]
nP = size(P_)[1]
@inbounds begin
Res = zeros(na,nh,nP,nj,np)
for a = 1:na
 for h = 1:nh
  for P = 1:nP
   for j = 1:nj
    for p = 1:np
     Res[a,h,P,j,p]= 0
     Res[a,h,P,j,p] =  Res[a,h,P,j,p].+log((A_[a].+H_[h].*P_[P + nP * (p + np * (j-1)-1)]))
    end
   end
  end
 end
end
end
return Res
end

# Generated tensor:
# T_Final V[a,h,P1,p1,j] * GP[P,P1] * Gp[p,p1,j] | a,h,P,p,j
function T_Final(GP,Gp,V)
np = size(V)[4]
na = size(V)[1]
nP = size(V)[3]
nh = size(V)[2]
nj = size(V)[5]
@inbounds begin
Res = zeros(na,nh,nP,np,nj)
for a = 1:na
 for h = 1:nh
  for P = 1:nP
   for p = 1:np
    for j = 1:nj
     Res[a,h,P,p,j]= 0
     for p1 = 1:np
      for P1 = 1:nP
       Res[a,h,P,p,j] =  Res[a,h,P,p,j].+V[a + na * (h + nh * (P1 + nP * (p1 + np * (j-1)-1)-1)-1)].*GP[P + nP * (P1-1)].*Gp[p + np * (p1 + np * (j-1)-1)]
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
