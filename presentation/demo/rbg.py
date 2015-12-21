# import math

# fl = open("rbgCurve.txt", "w")

# for i in range(256):
# 	x = 128*math.sin((2*math.pi/510)*i-math.pi/2)+128
# 	fl.write(str(int(x))+",")

# fl.close()

no_stch10 = [0.000008,0.000012,0.000011]
no_stch50 = [0.001084,0.001088,0.001143]
no_stch100 = [0.008240,0.007528,0.007596]
no_stch500 = [0.962742,0.966161,0.968154]
no_stch700 = [3.771401,4.183563,3.255337]
no_stch800 = [5.084043,4.877869,5.207951]

stch10 = [0.000726,0.000392,0.000659]
stch50 = [0.003135,0.002524,0.001826]
stch100 = [0.012736,0.011499,0.017954]
stch500 = [1.025202,1.150442,1.205838]
stch700 = [4.560645,3.541260,4.641537]
stch800 = [6.293257,5.917346,5.657210]

t10 = (sum(stch10)-sum(no_stch10))*100/sum(no_stch10)
t50 = (sum(stch50)-sum(no_stch50))*100/sum(no_stch50)
t100 = (sum(stch100)-sum(no_stch100))*100/sum(no_stch100)
t500 = (sum(stch500)-sum(no_stch500))*500/sum(no_stch500)
t700 = (sum(stch700)-sum(no_stch700))*700/sum(no_stch700)
t800 = (sum(stch800)-sum(no_stch800))*800/sum(no_stch800)

print(t10)
print(t50)
print(t100)
print(t500)
print(t700)
print(t800)

# % Stitch is higher than no Stitch
5632.25806452
125.791855204
80.5726759117
83.606397803
95.7332635404
142.279465543
