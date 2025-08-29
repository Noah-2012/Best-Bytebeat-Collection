# Best-Bytebeat-Collection
Here are the best Bytebeat Songs in one Readme. Have fun!\
Good composers are these:
 - [HTML5 Bytebeat](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjwpI-RsbCPAxWg1AIHHR7OAPcQFnoECAoQAQ&url=https%3A%2F%2Fgreggman.com%2Fdownloads%2Fexamples%2Fhtml5bytebeat%2Fhtml5bytebeat.html&usg=AOvVaw0KftetoqPGa4kz8kTZd6cb&opi=89978449)
 - [Bytebeat Composer](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwiMjYLRsbCPAxXzxQIHHYy8H_oQFnoECAoQAQ&url=https%3A%2F%2Fdollchan.net%2Fbytebeat%2F&usg=AOvVaw1GY2G0XSwmM10IiTTTgDRM&opi=89978449)

# Infix (Bytebeat)

## Small Bytebeat (< 256 Byte)

Name: t% funk\
Creator: lhphr\
From: https://dollchan.net/btb/res/3.html#66

```js
(t%(t&t>>12)/2**(t/1024%4-3)&127)+(8e3**(1-t%16384/1e4)&64)
```

---

Unnecessary-ARPEG.hc\
Creator: lhphr\
From: https://dollchan.net/btb/res/3.html#50

```js
((15+1*'1370'[t>>17&3])/8*'12346543'[t>>12&7]*(t/4)%192+160)%256/2+((sqrt(5E3*(t%65536&(2**[9,12,12,14,,,,,13,12,11,14][(t>>11&15|t^(t/32%8)*t)&11])-1))&128))
```

---

Name: The time is running out! More complicated drums\
Creator: SthephanShi

```js
t*(t&16384?7:5)*(3-(3&t>>9)+(3&t>>8))>>(3&-t>>(t%65536<59392?t&4096?2:16:2))|t>>3
```

---

Name: The time is running out!\
Creator: SthephanShi

```js
t*(t&16384?7:5)*(3-(3&t>>9)+(3&t>>8))>>(3&-t>>(t&4096?2:16))|t>>3
```

---

Name: The time is running out! Remix of RealZynx92's remix\
Creator SthephanShi\
From: https://www.reddit.com/r/bytebeat/comments/qf9meb/remix_of_the_time_is_running_out_by_stephanshi/hhzb9ft/?utm_source=reddit&utm_medium=web2x&context=3

```js
((t/2*(t&16384?7:5)*(3-(3&t>>9)+(3&t>>8))>>(3&-t>>(t%65536>59392?2:t&4096?2:16))|t>>4)&127)+(1E5/(t&4095)&64)+random()*((-t>>(t&4096?6:5))%64+64)
```

Name: a new industrial chiptune\
Creator: ryg\
From: http://www.pouet.net/topic.php?which=8357&page=11#c389005

```js
t*(1+'4451'[t>>13&3]/10)&t>>9+(.003*t&3)
```

---

Name: Neurofunk\
Creater: SthephanShi

```js
t*((t&4096?t%65536<59392?7:t&7:16)+(1&t>>14))>>(3&-t>>(t&2048?2:10))|t>>(t&16384?t&4096?10:3:2)
```

---

Name: Death chase "90's eurodance" variation\
Creator: SthephanShi

```js
t/=4,a=440,b=493.88,c=523.25,d=587.32,e=659.26,((t*[e,b,c,a,e,b,c,d][(t>>14)%8]/220*(3-(1&t>>9))>>(3&t>>8)|t>>[5,4,4,4,4,4,4,3,5,4,4,4,5,4,4,4][(t>>12)%16])&127)+(3E5/(t&4095)&64)+random()*((-t>>[6,5,5,5,4,5,5,5][(t>>12)%8])%64+64)
```

---

Name: remix of "I hear the long meowing of a cat :)" by SthephanShi\
Creator: RealZynx92\
From: https://www.reddit.com/r/bytebeat/comments/pz264e/remix_of_i_hear_the_long_meowing_of_a_cat/

```js
t*((t/2>>10|t%16*t>>8)&8*t>>12&18)|-(t/16)+64
```

---

Name: 2-bit song hardcore remix\
Creator: RealZynx92\
From: https://www.reddit.com/r/bytebeat/comments/qdfa0i/2bit_song/

```js
(sqrt((t&16383)*5000)&128)+(3E6/((t+16384)&32767)&128)+(t/2*(t&t>>11)&64)|(t/32&128)&t/16
```

---

Name: something Music
Creator: Decent-Manager-6169
From: https://www.reddit.com/r/bytebeat/comments/sc4v7u/something_music/

```js
ASDFDSA=t*[1.7,2.3,1.7,2.3,2.9,3.4,2.9,3.4][t>>13&7]*[1,0.884,1.317,1][t>>17&3]/4&63,EDCFCDE=t*[1,0.881,1.317,1][t>>17&3]/4,EDCFCDE=(EDCFCDE%75/2+EDCFCDE%75.1/2+EDCFCDE%56+EDCFCDE%44.5+EDCFCDE%37.5)/1.5,ASDFDSA+EDCFCDE
```

---

Name: something Music (Stereo Edition)\
Creator: Noadsch12 (me)\
From: https://www.reddit.com/r/bytebeat/comments/sc4v7u/something_music/

```js
[
  (t * [1.7,2.3,1.7,2.3,2.9,3.4,2.9,3.4][t >> 13 & 7] * [1,0.884,1.317,1][t >> 17 & 3] / 4 & 63)
  + (
    (t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 75 / 2
    + (t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 75.1 / 2
    + (t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 56
    + (t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 44.5
    + (t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 37.5
  ) / 1.5,

  ((t * [1.7,2.3,1.7,2.3,2.9,3.4,2.9,3.4][t >> 13 & 7] * [1,0.884,1.317,1][t >> 17 & 3] / 4 & 63) >> 1)
  + (
    ((t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 75 / 2
    + (t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 75.1 / 2
    + (t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 56
    + (t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 44.5
    + (t * [1,0.881,1.317,1][t >> 17 & 3] / 4) % 37.5
    ) / 1.5 >> 2 // can also be changed to 1.5 >> 0 for no pan effect
  )
]
```

---

Name: The fall of Derp Herp\
Creator: marmakoide\
From: http://www.pouet.net/topic.php?which=8357&page=10#c388980

```js
t*(0x13371337>>(t>>13&27)&127)|t>>4|t<<5
```

---

Name: music for a C64 game\
Creator: skurk\
From: http://www.pouet.net/topic.php?which=8357&page=8#c388896

```js
t*(t>>((t&4096)?((t*t)/4096):(t/4096)))|(t<<(t/256))|(t>>4)
```

---

Name: (none)\
Creator: ryg\
From: https://www.pouet.net/topic.php?which=8357&page=12#c389146

```js
((t*"36364689"[t>>13&7]/12)&128)+((((t>>12)^(t>>12)-2)%11*t/4|t>>13)&127)
```

---

Name: Noise Maker
Creator: Gabriel Miceli

```js
i=t&8191,(((t*((t>>9^((t>>9)-1)^1)%13)&255)/2)+((((t>>3|t<<(t>>12&2))*(i<4096)+(t>>4|t*(t^t+t/256))*(i>4095)))&255)/2)*(2+(t>>16))
```

---

## Medium Bytebeat (< 1 Kilobyte)

Name: CBRT-FILE R00TZ\
Creator: lhphr\
From https://dollchan.net/btb/res/3.html#50

```js
l=t%2**20,n=(l*l/Math.PI)%1,(l>>13)-31?((cbrt(6e7/(r='11141112'[l>>13&7])*(r*l&8191))&64)*1.5+(l>>13&1)*(l>2**17)*(l>>8*l&31)+(l>2**18)*(l*(m='45764586'[l>>15&7])*(5+1*'12357532'[l>>11&7])/20)%64)+(l>2**19)*(16*sin((l<<(r/2-1))*Math.PI/15435*44*(1*m+3))+16)+(l>786432)*((l>>14&1)*((~l+256>>8)%64+63)*n):((~l+128>>7)%64+95)*n
```

---

Name: TTT (The Tonal Tone)\
Creator: lhphr\
From: https://dollchan.net/btb/res/3.html#54

```js
p=t=>2**(t/12),s=t=>16*sin(f/t*q),a=(t,w,v)=>f/t*p([0,3,5,w,v][(5*f>>14)%5])&15,m=t=>f/7*p([0,0,,,3,5,0,0,-2,-2,,,-4,-4][((f>>14)-t)&15])*q&31,f=t*3.52%2**24,q=p([0,-4,0,3][f>>19&3]),n=((f/28*q&15)+(f/42*q&15)+(f/56*q&15))/(2**(2**15/(f%2**19))),(f>>22!=3?(f>>15)%256!=127?m(0)-(f>2**21)*(s(88)+s(176))+(f>2**22)*n+(f>2**23)*a(28/q,7,12):a(56,12,15)+a(28,12,15)+a(14,12,15):m(0)+m(4/3)/2+m(8/3)/4+m(4)/8+n+s(54)/3+s(88)/2)+128-(f>>19)
```

---

Name: ᓯᑯ ᓯᓚ ᑖᖅᑐᒥ ᐆᒻᒪᑦ\
Creator: lhphr\
From: https://dollchan.net/btb/res/3.html#54

```js
c=z=>t*2**(([5+z,3+z,2+z,z,5+z,3+z,10+z,7+z][t>>17&7]-5)/12),a=y=>t/2205*2**(('MQTWY]`cec`]YWTQ'.charCodeAt(t>>12&15)+[5,3,2,0,5,3,10,7][t>>17&7]-y)/12)%2.8,12*((c(0)>>4)%2.8-(t>2**20)*((c(o=1/20)>>5)%2.8+(2+(t>2**21))/3*(60620&1<<(t%2**17>>13)?(1-t%2**14/2**14)*sin(t**PI):0)))+(t>2**21)*((0xd9119111&1<<(t%2**18>>13)?22*sin(1024/(t%2**13)**.05):0)+12*((c(2*o)/64%2.8&c(2*o)/32.2)+(c(-o)>>3)%2.8/2-(a(o)+a(12))/2%2.8))+128
```

---

Name: Byte Me A Beat\
Creator: lhphr\
From: https://dollchan.net/btb/res/3.html#66

```js
t%=2**23,c=1*'00730079'[t>>16&7],m=(b,a,d)=>2**(b+(c+[4,7,4,7,6,4,2,0,4,,,4,,4,4][t>>12&15]-4)/12)*t%64>>(t>>a&d),n=(b=0,a=9,d=7)=>m(b++,a,d)-m(b,a,d),b=(a,d=256)=>16*sin(t/14e3*2**(8+(c+a)/12))**d,j=a=>28*sin(5e4/(t%2**(12+a))**1e-3)/(a+1)|0,k=0xd9119111&1<<(t%2**17>>12)?j(0):j(1),h=(8*((t>>12&3)==0)+8)*(1-t%2**12/2**12)*sin(t**3),d=b(-5)+b(0)+b(3)-2*b(-12,2),e=n()+n(-1,14,3)/2,z=(t>>15&15)!=15,min(255,[n(),n()-d,f=e-z*(d+h/2),q=4/3*((n(-1)&e)-e)-d+k+b(.05)+h,r=z*q-(n(0,11,1)-n(1,10,3))/(4-2*!z),g=(e^n())-d-2*b(-12,2)-h/2,z*g+n(-2)+e,q,r,i=e+n(-3,13)/2+2*b(-12,3)+k,z*i-d+h,q,r,g+k-h/2,e-d,f][t>>19]+128)
```

---

Name: Death chase As a basis is taken modified "The time is running out!" and "remix by RealZynx92" songs. Used the idea of selecting notes and rhythm through an array.\
Creator: SthephanShi

```js
t/=4,c=523.25,cs=554.36,e=659.26,g=784,((t*[cs,e,cs,g,cs,e,cs,c][(t>>14)%8]/(t&2<<16?440:220)*(3-(1&t>>9))>>(3&t>>8)|t>>[5,4,4,4,4,4,4,3,4,4,4,3,4,4,4,4,5,4,4,4,4,4,4,3,5,4,4,4,5,4,4,4][(t>>12)%32])&127)+(3E5/(t&4095)&64)+random()*((-t>>[6,5,4,5,5,5,4,5][(t>>12)%8])%64+64)
```

---

Name: bloop factory\
Creator: funute\
From: https://battleofthebits.org/arena/Entry/bloop+factory/20506/

```js
Z=int,P=pow,T=sin,I=parseInt,S=[0,2,4,7,9,5,6,19,12,1],B=[0,4,5,4,0,-4,-5,-7],n=function(c,e){return 127*P(T(c*P(1.05946,e)/15.9517),3)},r=function(c){return I(T(c).toString(16).substring(7,9)||0,16)-128},X=function(c){return I(T(c+.1).toFixed(6)[5])},a=function(c,e){return c*e},d=5E3,b=Z(t/d),p=1-t%d/d,W=2*d,Y=Z(b/2),L=max(t-3*d,0),C=Z(L/W),F=1-L%W/W,N=B[Z(Y/16)%8],G=4*d,R=P(1-t%G/G,3),a(r(t),P(p/2,3)+P(2==b%4&&p,.5)/7)+a(n(t,S[X(Y%4+Z(Y/16))]+N),(1-t%W/W)/4)+a(n(L,S[X(C%4+Z(C/16))]+B[Z(C/16)%8]),F/16)+a(0<T(99*R)?19:-19,R)+a(r(Z(t/4)),Z(t/G)%2&&R/4)+a(n(t,N-48),.25)+128
```

---

Name: Last Palette\
Creator: pigdevil2010\
From: https://battleofthebits.org/arena/Entry/Last+Palette/25369/

```js
(((t<<(4-(t*3>>13&1)))/("@Lf@DUfD".charCodeAt(t*3>>14&7))|(t*3>>9))&31)+
(((t<<(4-(t*3>>13&1)))/("3@L39DU93@L39D+9".charCodeAt(t*3>>14&15))|(t*3>>9))*(254>>(t*3>>20&7)&1)&31)+
(((t<<2)/("LL`rLL`U".charCodeAt(t*3>>18&7))|6)*(254>>(t*3>>21&7)&1)&31)+
(((t<<2)/("MMasMMaV".charCodeAt(t*3>>18&7))|6)*(254>>(t*3>>21&7)&1)&31)+
(((t<<4)/("3@Lf".charCodeAt(t>>11&3))|(-t*3>>12&7))*(22235>>(t*3>>14&15)&1)*(126>>(t*3>>21&7)&1)&15)+
((((t*t/21+t&(t>>3))|t*3>>9|t*3>>10)-((t*3>>16&1)*9))*(6>>(t*3>>22&3)&1)&63)+
(((t<<5)/("LLLD@DUfLLL3DD+933L3&+39@@f@DDUf".charCodeAt(t*3>>16&31))^(t*3>>11)|(t*3>>14&15))*(104>>(t*3>>21&7)&1)&31)+
(((t<<3)/("393+&+3@030&+33+&@3+ \"&+03@3+33@".charCodeAt(t*3>>16&31))^(-t*3>>11)|(-t*3>>14&15))*(104>>(t*3>>21&7)&1)&15)
```

---

Name: Last Fountain\
Creator: pigdevil2010\
From: https://battleofthebits.org/arena/Entry/Last+Fountain/27281/

```js
s=(a,b,c)=>c/a.charCodeAt(b),d=a=>a&1?-1:1,e=(a,b)=>t>>22&1?b:a,b=a=>(s(a,r(t)+p*16,t<<2)*(254>>(t>>20&7)&1))&20,
p=178>>(t>>19&7)&1,q=a=>120>>(a>>20&7)&1?1:(-a>>16&1),r=a=>112>>(a>>20&7)&1?t>>14&15:(t>>17&1)*8,
l="  QQffQQLLLfLLDDQQQfff  rrff``UU  QQDDQQLLLfLLDD<<f333  r\x98rf`r`U  <<<9<LUUU\x80U\x80UL[[rLLL  rrff`frr  <<<93U999`9U3+--9&&&  rrff``UU",
b("rrLr99rrUUrU99rULLyL<<yy``\x80`@@\x80`")*2+b("qqKq88qqTTqT88qTKKxK;;xx__\x7f_??\x7f_")+
(s("rf[<r`L@\x98\x88yQ\x80fUL",(t&3)+(t>>17&1)*4+p*8,t<<2)&7)*(t*d(t>>16)>>12&15^e(0,5))*3/4+
((s("rf[L<9-\x1e&-3&-3-3\xab\x98\x90r`UH0+&9+&\x1d&+\x98\x88yfQL<(3<D3<3-(&09HL`ULU`r`UL@9",(t>>13&31)+p*32,t<<5-(t>>11&3))|t>>8)*q(t)&31)+
(((s(l,t>>14&127,t<<6)&s(l,t>>14&127,(t*e(89/88,499/498))<<6))*(63486>>(t>>15&15)&1)*(102>>(t>>20&7)&1))&e(42,32))+
((((253507989>>(t>>6&31))*(1>>(t>>11&3))*(19593>>(t>>13&15)&1)&1)*50)+
((((t*t/21+t&(t>>3))|t>>7|t>>8)-7)*(3>>(t>>11&3)&1)*(2450526224>>(t>>13&31)&1)&31)*5/2)*(112>>(t>>20&7)&1)
```

---

Name: the proto-version\
Creator: Bp103\
From: https://twitter.com/Bp103/status/934396842850291712

```js
r=44100,q=t/r,a=[[261.63,329.63,392],[261.63,392,349.23],[293.66,440,349.23],[329.63,392,523.25]],b=int((q*.75)%4),c=(((q*a[b][0])%1)-((q*a[b][1])%1)-((q*a[b][2])&1))>.05,l=((q*[174.61,196,164.81,130.81,146.83,164.81,146.83,130.81][int((q*4.50)%8)])%.5)>.05,(l>c)*16
```

---

Name: a sequencer type thing\
Creator: Bp103\
From: https://www.reddit.com/r/bytebeat/comments/enmbix/made_a_sequencer_type_thing/

```js
q=t/32e3,r=1.05946309,a=880,as=a*r,b=as*r,c=b*r,cs=c*r,d=cs*r,ds=d*r,e=ds*r,f=e*r,fs=f*r,g=fs*r,gs=g*r,

na=(q*[
//arp chords
a,c,e,
a,d,f,
a,c,f,
c,e,g,

g,b,d,
f,a,c,
a,c,g,
g,b,d,

][(int(q*1)%8)  *3+ (int(q*32)%3) ])%2,
nb=(q*[
//lead

a,b,c,e ,c,b,a,0
][int(q*4)%8]%2),

nc=(q*([
//BASS

a,a<<1,a,0,a,0,a,0, g>>1,g,g>>1,0,e>>1,0,c>>1,c 
][int(q*4)%16]>>2)%4),

kick=((q*(512 >> (q*64)))%2)<<6,
snare=((q*4)&3)>1?q*(8e3>>q*64)%2<<5<<((sin((q*2e5)>>5)*(q*2e5))<<(q*64)):0,
hihat=((q*64)%8)<1?((q*7040)%2)<<4:0,

//output channels
  (na<<4) 
+ (nb<<4) 
+ (nc<<4) 
+ kick + snare + hihat;
```

---

Name: now its FM synth\
Creator: Bp103\
From: https://twitter.com/Bp103/status/1345905059964530688

```js
q=t/32e3,r=1.05946309,a=880,as=a*r,b=as*r,c=b*r,cs=c*r,d=cs*r,ds=d*r,e=ds*r,f=e*r,fs=f*r,g=fs*r,gs=g*r,
//Coded and Composed by Bp103

na=(q*[
//arp chords
a,c,e,
a,d,f,
a,c,f,
c,e,g,

g,b,d,
f,a,c,
a,c,g,
g,b,d,
][(int(q*1)%8)  *3+ (int(q*32)%3) ]),

tr=((int(q*4)%128)<64)?0:1,
nb=(q*[
//arp chords
a<<(tr),c         ,e<<(tr),c<<(tr<<1),
a<<(tr),d<<(tr<<1),f<<(tr),d         ,
a<<(tr),c         ,f<<(tr),a<<(tr<<1),
c<<(tr),e<<(tr<<1),g<<(tr),e         ,
g<<(tr),b         ,d<<(tr),b         , //phrase 2
f<<(tr),a<<(tr<<1),c<<(tr),a<<(tr<<1),
a<<(tr),c         ,g<<(tr),c         ,
g<<(tr),b<<(tr<<1),d<<(tr),b<<(tr<<1),

][int((q*4)%32)])/2,

nc=(q*[
//Lead
a,b,c,e ,c,b,a,0]
[int(q*4)%8]),

nd=q*([
//BASS
a,a<<1,a,0,a,0,a,0, g>>1,g,g>>1,0,e>>1,0,c>>1,c 
][int(q*4)%16])/4,

//kick = tan(sin( (q* ((0.4/((q*4)%2)))/200)) ) * (32- (((q*32)%32)) )/1.5,
kick=((q*(512 >> (q*64)))%2)<<6,
snare=((q*4)&3)>1? (q*(8e3>>q*64)%2<<5<<(((q*2e5)>>5*(q*2e5))<<(q*64)))&127:0,
hihat=((q*64)%8)<1?((q*7040)%2)<<4:0,

//output channels

//kick+
((kick+snare+hihat)/2)+
(((sin(nc*1)*3) * (sin(nc*4.0)*4) * (sin(nc*1.02)*2))/2) + 
((tan(sin(nd/2)*tan(cos(nd*1.5)*1.03))+4)*3) +
((sin(na*4)<cos(q*6))*4.5) +
((tan(sin(nb*2.00)*1.5)/2.39) * (sin(nb*3.00)*3) * (cos(nb*1.013)*2)/2) +96;
```

---

Name: A NΞW DIMΞNSION\
Creator: burlynn n01se\
From: https://battleofthebits.org/arena/Entry/A+N%CE%9EW+DIM%CE%9ENSION/42606/

```js
wv=function(x){y=x%256;return y<127?y:2*(256-y)},
ms=floor(t/10000),
p=t/10000%16,
b=p%4,
qn=p%1,
rh=function(a){m=0;for(i=0;i<a.length;i++){if(b>a[i]){m=b-a[i]}}return m},
sc=(ms%256<32)?1:min(1,max(0,3*qn-0.3))*(ms%32>0),
c=t*(p<12?2:2.4),
max(0,min(255,
(
sc*(
(wv(t/4)-wv(t/2+qn*wv(t*(ms>>2&15))+wv(t*2*(1+(ms>>5&3)))))*(1-rh([0,1,1.5,2,2.75,3.5]))*(ms%256>63)

-wv(t/2+0.5*wv(pow(7-(qn*(2+(ms>>2&3)))%1,5)*3))*(ms%64>47)

+qn*qn*((t<<(2+(t>>9)%3))%256)*(ms%128>64)*0.8

-wv(pow(2-rh([0.25,1,1.5,2.25,3]),4)*6000*(ms%256>191?2:1))*(ms%256>128)

+(wv(c)-wv(c*1.2)+wv(c*1.5)-wv(c*2.24)+wv(c*1.8))*(0.7-rh([0,.75,1.5,2.5,3.25]))*1

+random()*pow(16-p,2)/4
)*0.3

-qn*pow(1-qn*(ms%128<80?2:4)%1,4)*80*random()*(ms%256>63)
)
+wv(2*t+60*sin(b*25))*(ms%256>31)*(ms%32<1)*qn

+wv(pow(1-qn,9)*2000*(1+p%2))*(max(0,1-5*qn))*(ms%256>31)

+128
))
```

---

Name: 511\
Creator: johanvandegriff\
From: https://www.reddit.com/r/bytebeat/comments/o7e957/511/

```js
i=int,z=sin,c='charCodeAt', //set up shortcuts for the int, sin, and charCodeAt functions for shorter code later
b=t/441e3*16, //scale the time variable by the sample rate (44.1kHz)
m=1<<((b/16)%16), //a one-hot variable to tell which measure the song is in, used to play different things in different measures
q=b%.5, //repeating variable from 0 to 0.5, used for kick drum and white noise for snare
h=x=>t/2210*2**(x/12-3), //sawtooth waveform and note frequency calculation
w=(x,y=0)=>h(x)%1-z(y*2+b*PI)/8>.5, //sine waveform, uses 'z' as a shortcut for 'sin' as defined on the first line
s=i(b*2)%32, //which count within the measure we are at
a=abs(8-i(b*8)%16), //which sub-count in the measure
p='pnkgrnkipmigpnkg', //a sequence of notes used in the arp and the last melody
99+27*( //multiply by 27 to make it loud enough and add a constant to avoid distortion
//to sequence the parts, the "m&0b0000011100111111" and similar only plays the part during certain measures. 1st measure is on the right
(m&0b0000011100111111&&0b10100110100001&1<<s%16?z(q/(q/241+2e-4))+z(t/140)/2:0)+ //kick drum. beats appear as 1's in "0b10100110100001" starting from the right
(m&0b0001110101011111?z(t**3/q)*5**(-3*((b+1)%2)):0)+ //snare drum. sin(t**3/q) creates white noise, and 5**(-3*...) makes the sound decay
(m&0b0010011011011000?w(s==6&&i(b*6%6)==1?85:s==14&&q<.25?78:'SSSSSSSQOOONNOLJLLLLLNLJIIIGGG'[c](s),s==5):0)+ //square wave melody with a twiddle
(m&0b0001110010011110?w('@@@@@@BCEEECCBCCEEEEECEEBBB@@@'[c](s)):0)+ //square wave bassline. the last few @'s are removed so it cuts out at the end of the measure
(m&0b0110111001111100?(h(p[c](a%4+i(s/8)*4)+12*-i(a/4))%1):0)+ //sawtooth arpeggiator, loops thru each sequence of 4 notes quickly twice before moving to the next 4
(m&0b0001001100110000?(z(PI*h(p[c](s*3%16))))**3:0) //slower sine wave arpeggiator. re-uses the same note list but plays every 3rd note and wraps around
)
```

---

Name: probably the best song i ever made
Creator: RealZynx92
From: https://www.reddit.com/r/bytebeat/comments/q8r7g4/probably_the_best_song_i_ever_made/

```js
a=t/[1.5,1.25,1.5,1][(t>>17)%4]*((t>>12)+1&1),(((a/4^(.998*a/8))&63)%(t>>14)+((32*sin((t/[1,1,1,2,1.5,1.5,1.25,1.25][(t>>15)%8]/20.5))+32)*(t>>20>0)))*(t>>15!=63)+(t*random()%((-t>>9)%64+64)*(t>>15==63))+(((3E5/(t&8191)&50)*[1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1][(t>>13)%16])+((3E6/(t&8191)&45)*[0,0,0,0,1,0,0,0][(t>>13)%8])+((t/4*(t&t>>12))&32))*(t>>20>1)
```

---

Name: Melody Of Im Blue (with drums)\
Creator: Decent-Manager-6169\
From: https://www.reddit.com/r/bytebeat/comments/rs1qqi/comment/hqridb8/?utm_source=reddit&utm_medium=web2x&context=3

```js
a=t*[1.35,0.85,1.125,1.35,1.5,1,1.27,1.35,1.35,1.125,1.35,1.7,1.8,1.125,1.7,1.5,1.35,0.85,1.125,1.35,1.5,1,1.27,1.35,1.35,1.125,1.35,1.7,1.8,1.125,1.7,1.5,1.35,0.85,1.125,1.35,1.5,1,1.27,1.35,1.35,1.125,1.35,1.7,1.8,1.125,1.7,1.5,1.35,0.85,1.125,1.35,1.27,0.75,1,1.125,1.125,1.125,1,1.125,1.125,1,1.125,1.27][t/2.4>>12&63]/4&63,
b=(Math.sin(t/2|t/12)*((-t/2.4>>6&31)+1)+32)*[1,0][t/2.4>>11&1],
c=(Math.sin(t/3|t/18)*((-t/2.4>>8&31)+1)+32)*[0,1][t/2.4>>13&1],
a+b+c
```

---

Name: Doom E1M1 theme recreation\
Creator: PortablePorcelain\
From: https://dollchan.net/btb/res/3.html#55

```js
q='5 5 JJ5 5 FF5 5 AA5 5 ==5 5 ==??5 5 JJ5 5 FF5 5 AA5 5 ========  5 5 JJ5 5 FF5 5 AA5 5 ==5 5 ==??5 5 JJ5 5 FF5 5 AA5 5 ========  < < XX< < RR< < MM< < II< < IILL< < XX< < RR< < MM< < IIIIIIII  5 5 JJ5 5 FF5 5 AA5 5 ==5 5 ==??5 5 JJ5 5 FF5 5 AA5 5 ========  ',s=((t/540)%q.length),b=(q.charCodeAt(s)-32)*(t/8),tanb=tan(b*Math.PI/512)*64-128,sinb=sin(b*Math.PI/64)*64-128,(tanb|sinb)-sinb
```

---

Name: TIRO remix\
Creator: PortablePorcelain\
From: https://dollchan.net/btb/res/3.html#80

```js
t/=4,z=(x)=>log(sin(x*PI/64)/1+4),y=(x)=>z(x*(1+(1/(x/x))))-sin(x*PI/(64-(1/(x/t))))/2,c=(x)=>abs(y(x/2))**y(x)/z(x),q=c(t*2/(2**(-t>>16&3))*(t&16384?7:5)*(3-(3&t>>9)+(3&t>>8)))*(t&4096?16:36),b=z(t&4096?(3-(3&t>>9)+(3&t>>8))*t*(t&16384?7:5)>>3&t*(t&16384?7:5):0)*128,l=3e4/(t%(t&8192?3072:4096))&72,k=b+q+l,u=(random()*64|t>>"6554554455545434655455445554543265545544555454346554554455543210"[t>>12&63])%64,g=k+u,j=5e4/(t%(t&8192?7168:6144))&64,n=(g+j-164),s=z(t/4*(t&16384?7:5)|t>>13)*64,min((s+n)/1.5,255)
```

---

Name: arpeggiator fun\
Creator: SArpnt\
From: https://www.reddit.com/r/bytebeat/comments/fjsa1y/arpeggiator_fun/

```js
i=int,c='charCodeAt',b=t/441e3*16,q=b%.5,h=x=>t/2210*2**((x-36)/12)%1,w=(x,y=0)=>(h(x)-sin(y*2+b*PI)/8>.5)*30,s=i(b*2)%32,a=abs(8-i(b*8)%16),t==0&&(v=.5),v=3.65*v*(1-v),sin(q/(q/100+3e-4))*40*!(54878&1<<s%16)+(v-.5)*70*max(0,1-((b+1)%2)*.77)+w(s==6&&i(b*6%6)==1?85:s==14&&q<.25?78:'SSSSSSSQOOONNOLJLLLLLNLJIIIGGGGG'[c](s),s==5)+w('@@@@@@BCEEECCBCCEEEEECEEBBB@@@@@'[c](s))+h('pnkgrnkipmigpnkg'[c](a%4+i(s/8)*4)+12*-i(a/4))*27+99
```

---

Name: squsinsaw music\
Creator: _elevate__\
From: https://www.reddit.com/r/bytebeat/comments/s7aiep/squsinsaw_music/

```js
squ=((t)*[1,0,2,0,1,0,2,2,1,0,2,0,1,0,2,0,1.5,0,3,3,1.5,0,3,0,1.35,0,2.7,2.7,1.35,0,2.70,][t>>10&31])&64,sine=((sin(((t/40.743648)*([8,9,9.5,12][t>>10&3])*[1,1,1.5,1.35][t>>13&3]))*32)+32),saw=(((t/4)*[0,0,0,0,0,0,0,0,8,9.5,12,16,0,0,0,0][t>>8&15])&31),kick=((1E5/(t&4095)/5)&63),snr=((((t*t*t>>10)&63)|((t>>6)&63))*"0101"[t>>12&3]),(sine%((-t>>4)&63))+(squ*[0,1,1,1,1,1,1,1][t>>16&7])+(kick*[0,0,1,1,1,1,1,1][t>>16&7])+(snr*[0,0,1,1,1,1,1,1][t>>16&7])+(saw*[0,0,1,1,1,1,1,1][t>>16&7])
```

---

Name: Awesome level - Remixed\
Creator: NewFall2020\
From: https://www.reddit.com/r/bytebeat/comments/r557yi/4_links/

```js
t/=4,
a=(t*(t&16384?6:5)*(3+(3&t>>(t&2048?7:16/2)))>>(3&t>>10)|t>>4),
b=(t*(t&16384?6:5)*(3+(3&t>>(t&2048?7:14)))>>(3&t>>9)|t>>4),
c=3e4/(t&4095)&128-64,
d=(t*(t&16384?6:5)*(2*(1&t>>11)))/32%64,
e=(random()*(((-t>>[6,5,5,5,5,5,5,4,5,5,5,4,5,5,4,3][(t>>11)%16])%64)+64)),
(((((a%64)+(a/2%64)+(b/4%64))/2)+64)+(c/2*2)-44)+d+e
```
---
# Postfix(rpn) (Bytebeat)

## Small Bytebeat (< 256 Byte)

Name: guitarhead from Glitch Machine

```js
t 5 * 3 >> t &
t 4096 % 1024 < &
t 12 >> 19 & 1 + 25 * PICK 208 % 3 *
+ + 4 / 255 &
DUP
DUP
4 *
```

---

# Glitch (Bytebeat)

## Small Bytebeat (< 256 Byte)

Name: pipe_symphony from glitch machine

```js
pipe_symphony!aEk5h5f!a11k2h!a9k3hdf!aDk4hg!ad4e!p5fm!a11k2h1rg!a5kdm
```

---

# Infix (Floatbeat)

## Small Floatbeat (< 256 Byte)

Name: ????2\
Creator: (Unknown)

```js
((  (t>>7|t|t>>6)*10+4*(t&t>>13|t>>6)                        ) & 255) / 127 - 1
```

---

Name: ????5\
Creator: (Unknown)

```js
((  ((t*5)&(t>>7)) | ((t*3)&(t>>10))                         ) & 255) / 127 - 1
```

---

Name: Mouse Position\
Creator: (Unknown)

```js
// Move the mouse around rapidly.

(sin(t * 0.0001 * mouseX) +
 cos(t * 0.0003 * mouseY)) * 0.5
```

---

Name: happy floatbeat melody
Creator: RealZynx92
From: https://www.reddit.com/r/bytebeat/comments/rtkgtn/happy_floatbeat_melody/

```js
a=t*[1,1,1,1,1.5,1.5,1.33,1.33][t>>15&7],b=t*[1,1.33][t>>14&1],sin(t*"6868686834343434"[t>>13&15]/41)/8+(b*4%256/256)/4+atan(tan(a/41)+cos(a/100))/[4,8,16,32][t>>13&3]
```

---

## Medium Floatbeat (< 1 Kilobyte)

Name: Sinusoid Pusher\
Creator: lhphr\
From: https://dollchan.net/btb/res/3.html#88\

```js
q=(f,v,y)=>(2*y-1)*(f(t*(t&t>>12-log2(1+(z=t>>21&3)))/v/(1+z)+y*PI)**(1-(g=z!=t>>21)/2)),k=y=>(2*y-1)*(sin(2e3/(t%16384)**.1+y*PI)**.25),c=h=>isNaN(h)?0:h,((c(q(cos,1e5,0))+g*c(q(cos,1e5,1)))*(c(q(sin,7e4,0))+g*c(q(sin,7e4,1)))+sin(t%256/(6-4*g)**(t/512%64))+g*(c(k(0))+c(k(1)))/2+sin(2*PI*random())/(2-g/2)**((4+t/4096)%8))/2
```

---

Name: Game of Thrones theme\
Creator: Wiebe-Marten Wijnja\
From: https://js1k.com/2014-dragons/details/1953

```js
v=t>>12,//v = check at what section we are
v%=1152,//repeat song

//patterns:
//NOTE: some patterns that are commented out here are placed inside the synth scripts themselves, as they don't need any post-processing
D="881146",//start theme
//X=D.replace(/4/g,5),//start theme major
//F="1       1 1 ",//drums
G="888888111111468888111146",//couplet 1
//I=G.replace(/4/g,5),//couplet 1b (major)
//J="8=",//base for intro
//L="==;;1188--468888",//chorus
//N="==449;==449;;;4489;;4489881146881146881146881146",


M = function( p,o, q, m,s){
	
	g=t*pow(2, (m.charCodeAt(q)+p)/12-o);//get absolute pitch from semitone.
	x=(g%128)/64-1;//This section is used by both saw and triangle wave (as tri is nothing more than abs(saw) )
		return (s?(s<2)?x:abs(x):(g&128)/64-1);//The real magic: decide between pulse, saw and triangle and synthesize them.
},

0.2*((v<192?M(0,4,(v>>1)%6, ((v%96)<48?D:D.replace(/4/g,5)),2)+(v%6?0:M(0,5,0,"1",0))://intro
v<192*2?((v%96)<48?M((v%192<96?12:10),6,(v>>1)%24, (v%192<96?G:G.replace(/4/g,5)),2):M((v%192<96?7:5),6,(v>>1)%6, D,2))+M((v%192<96?12:10),7,(v%96)<48,"8=",2)://first part
v<192*3?((v%96)<48?M((v%192<96?12:10),6,(v>>1)%24, (v%192<96?G:G.replace(/4/g,5)),0):M((v%192<96?7:5),6,(v>>1)%6, D,1))+M((v%192<96?12:10),7,(v%96)<48,"8=",2)://first part repeated with different synths
v<192*4?((v%96)<48?M((v%192<96?12:10),6,(v>>1)%24, (v%192<96?G:G.replace(/4/g,5)),1):M((v%192<96?7:5),6,(v>>1)%6, D,1))+M((v%192<96?12:10),7,(v%96)<48,"8=",2)://first part repeated with different synths

+M(0,5,(v>>1)%48, "==449;==449;;;4489;;4489881146881146881146881146",0)+M(0,6,((v/12)%16),"==;;1188--468888",1))//chorus


//This number is the reverse binary representation of 100100010001000000000001010101000001010100000001, which is the drums pattern.
//By doing num>>[0-48] every time, it loops through the pattern. If it finds a one, it plays noise. Otherwise nothing is heard.
+(159497927791873>>((v)%48)&1?random():0))
```

---

Name: Floatbeat test\
Creator: lehandsomeguy

```js
time = t/32000,
PI = 3.14159265358979323,
fract=function(x) {
    return x%1
},
mix=function(a,b,c) {
    return (a*(1-c))+(b*c)
},
tri=function(x) {
    return asin(sin(x))/(PI/2.)
},
puls=function(x) {
    return (floor(sin(x))+0.5)*2.;
},
saw=function(x) {
    return (fract((x/2.)/PI)-0.5)*2.;
},
noise=function(x) {
    return sin((x+10)*sin(pow((x+10),fract(x)+10)));
},
melodytest=function(time) {
	melody_string = "5789357857893572";
	melody = 0;
	for (var i = 0; i < 5; i++) {
	melody += tri(time*mix(200+(i*900),600+(i*900),(melody_string.charAt(floor(time*2)%melody_string.length))/9))*(1-fract(time*4));
	}
	return melody;
}
,
hihat = noise(time)*pow(1-fract(time*4),4),
kick = sin(pow(1-fract(time*2),5)*100),
snare = noise(floor(time*9000)/9000)*pow(1-fract(time+.5),6),
melody = melodytest(time)*pow(fract(time*2),2)*2,
(hihat+kick+snare+melody)/4
```

---

Name: Trippy test\
Creator: lehandsomeguy

```js
time = t/44100,
PI = 3.14159265358979323,
fract=function(x) {
    return x%1
},
mix=function(a,b,c) {
    return (a*(1-c))+(b*c)
},
mod=function(a,b) {
    return a%b;
}
,
clamp=function(a,b,c) {
    return max(min(a,c),b);
}
,
tri=function(x) {
    return asin(sin(x))/(PI/2.)
},
puls=function(x) {
    return (floor(sin(x))+0.5)*2.;
},
saw=function(x) {
    return (fract((x/2.)/PI)-0.5)*2.;
},
noise=function(x) {
    return sin((x+10)*sin(pow((x+10),fract(x)+10)));
},
main=function(x) {
	s = 0;
	for (i = 0; i < 10; i++) {
	s += tri(time*(1000+i+(mod(floor((time+(i*.02))*8),16)*(250+(i*3)))));
	}
	s /= 9;
	s += sin(time*250)*.5;
	s += sin(pow(1-fract(time*2),10)*100);
	s += noise(time)*(1-pow(fract(time*8),.2+(.2*(mod(time,4)<.5))))*.5;
	return s*.7;
}
,
main(time);
```
---
## Large Floatbeat (> 1 Kilobyte)

Name: Cover of 'Focus' from SuperHexagon\
Creator: Wiebe-Marten Wijnja (Original by: Niamh Houston)

**WARNING:** I know it looks wierd but it works on any composer!

```js
====================================================================
====================================================================
		   /       _____       \
		  /       /     \       \
		 /       /       \       \
		(       (         )       )
		 \       \       /       /
		  \       \_____/       /
		   \                   /
			   FOCUS
			HEXAGONEST
		      SUPER HEXAGON
							      v1.4
====================================================================
====================================================================
Bytebeat version of "Focus", made by DoubleyouDashM  (Wiebe-Marten Wijnja)
	My site:  http://www.wmmusic.nl

Original song written by Niamh Houston (Chipzel)
	Her site: http://chipzelmusic.bandcamp.com/

for the game Super Hexagon, which was made by Terry Cavanagh
	His site: http://distractionware.com/

===

Note: Source is up on GitHub: https://github.com/Qqwy/ExtremeBytebeats

Why did I make this?
-> I love Super Hexagon
-> I love Chipzel's Music and Chiptune in general
-> I wanted to learn more about and Waveform creation in computers
-> I wanted to learn more about procedural content genration. (Yep, although the output from this code is deterministic, it is procedural)


Features:
->Speedup or slowdown as much as you want without losing quality
->Play backwards!
->When compressed, only 3452 bytes:  1/1000th of the size of Focus as .mp3(actually a lossy format) which is +- 2.5 Megabytes.
	(Note that further compression should also be easily possible. There's lots here that could be code-golfed)



==
This was lots of hard work. I hope you like it.
Now I'll go back to actually beating the game! ;D
~W-M

*/

//Some values for you to tamper with:

backwards = false,	//Enable this for sdrawkcab fun!
factor=1.055, 		//change this value to increase or decrease speed. 1.055 is the speed the original Hexagonest has.
speedup = true, 	//Song will increase its speed slightly while going on! :D



/*

*/



factor+=(speedup?t*.000000001:0),
//Ensure pitch correction:
!hq&&(t*=5.6),
//Play backwards when enabled
backwards&&(t=(8<<20)-t),


t*=factor,




t=t%(8<<20),//Song looping

//Pseudo random number generation using trigonometry
rand = function(t){
	with(Math){
		return cos(t*cos(t)); 
	}
},

//Used for Hihat and Snare
noise=function(ocshift, envelope, espeed, eshiftspeed, emod, gain){
	return ((rand(t>>ocshift)) *gain *(envelope?ge(envelope, espeed, eshiftspeed, emod):1)||0);
},


//Saw wave, used for most Bass instruments
saw=function(melody, mspeed, mmod, ocshift, envelope, espeed, eshiftspeed, emod, gain){
	
	return ((((t/factor)*Math.pow(2, gm(melody, mspeed, mmod)/12-ocshift)%255)/127-1)*gain   *(envelope?ge(envelope, espeed, eshiftspeed, emod):1)||0)
},


//Triangle wave, used for most melody instruments
tri=function(melody, mspeed, mmod, ocshift, envelope, espeed, eshiftspeed, emod, gain){
	return ((abs((t/factor)*Math.pow(2, gm(melody, mspeed, mmod)/12-ocshift)%4 -2)-1)*gain   *(envelope?ge(envelope, espeed, eshiftspeed, emod):1)||0)
},


//Pulse wave, used for some of the instruments
pulse=function(melody, mspeed, mmod, ocshift, envelope, espeed, eshiftspeed, emod, gain){
	return ((((t/factor)*Math.pow(2, gm(melody, mspeed, mmod)/12-ocshift)&128)/128)*gain   *(envelope?ge(envelope, espeed, eshiftspeed, emod):1)||0)
},


//Sine wave. Actually unused right now, but I thought i'd keep it here for documentation.
sine=function(melody, mspeed, mmod, ocshift, envelope, espeed, eshiftspeed, emod, gain){
	
	return (sin((t/factor)*Math.pow(2, gm(melody, mspeed, mmod)/12-ocshift))*gain   *(envelope?ge(envelope, espeed, eshiftspeed, emod):1)||0)
},


//Sine wave with support for pitch-shifting. Used for some fill-in effects. Poiiiing!
jump=function(melody, melody2, jumpspeed, mspeed, mmod, ocshift, envelope, espeed, eshiftspeed, emod, gain){
	var d=gm(melody, mspeed, mmod);
	var e=gm(melody2, mspeed, mmod);
	var g=((e-d)*(((t)%(jumpspeed))/(jumpspeed)))  +d;
	var x=t;
	return (sin(((t)%jumpspeed)*Math.pow(2, g/12-ocshift)    	)*gain   *(envelope?ge(envelope, espeed, eshiftspeed, emod):1)||0)
},

rkick=function(ocshift, envelope, espeed, eshiftspeed, emod, gain){
	return (((sqrt(t%0x2000)<<6  &255)/127-1) *gain *(envelope?ge(envelope, espeed, eshiftspeed, emod):1)||0);
},


//Get Melody function. Returns the proper current tone from the melody string.
	//Tone height in semitones is the ASCII value of the current char.

gm=function(m, speed, mod){//Get Melody
	var d=m.charCodeAt((t>>speed)%mod);
	return d==32?0:d;//Space is rest(although it has to be set in the envelope as well to prevent clicks)
},


//Get envelope function. Returns the proper current envelope speed from the envelope string.
	//All envelopes are simple Decay envelopes. The numbers indicate the time it takes to go from full volume to 0.
	//9 indicates to keep full volume the whole time.
	//0 indicates a rest.

ge=function(e, espeed, eshiftspeed, mod){
	en=e[(t>>eshiftspeed)%mod];
	var d=espeed * +en;
		return (en==9?.4:(1-(t%d/d)))
},

/*
========
PATTERNS
========
*/

m="ADCHAAAA", 
me="88424444",

b1m=   "AAAAYMAMAAAAYMAMAAAAYMAMAAAAYMAM====UM=M====UM=M????WM?M????WM?M",
b1e="88882222",

b2m="AAAAAAAC====????",
b2e=(t>>20==0?"99":"02"),

bell1m="AACCDDAAAACCDDAAAACCDDAAAACCDDAAAAHHKKFFAAHHKKFFFFHHMMKKKPOKMM  ",
bell1e="3030303030303030303030303030303030303030303030303030303033333300",

bell2m="ACDF",
bell2e="02202000",

m2= "=< 5 CD ",
m2e="22020220",

j= "####8888####8888####8888####8D#&",
jb="----DDDD----DDDD----DDDD----DQ&!",
je="2",

m3= "D5CD?5C5",
m3e="42224242",

b3m="AA=?AAAA",
b3e="2",

m4="AAHDDDDC",
m4e="40",

m5= "AHAM MHMAHAM MHMDKDP PDPCJCO OJO?F?K KFK?F?K KFKAHAM MHM<C<H HCH",
m5e="22240222",

b4m= "AA=?",
b4e="2221220000000000",

j2= "#",
j2b=")",
j2e="02000000",

m6= "HD A IM HD A IM HD A IM HD A IM DHCD?ACADHCD?ACADHCD?ACADHCD?ACA",
m6e="22 2 22 22 2 22 22 2 22 22 2 22 22222222222222222222222222222222",

m7="HAAA",
m7e="1",

m8="AAHDDDCC",
m8e="1000",

base= ((t>>20==0)||(t>>20==2)?"F F  F  ":(t>>20==1)?"F F F F ":(t>>20==3)?"        ":(t>>20==4)?"F   F   F F F   ":"F F  F  "),
basee=((t>>20==0)||(t>>20==2)?"20200200":(t>>20==1)?"20202020":(t>>20==3)?"00000000":(t>>20==4)?"2000200020202000":"20200200"),

//Extra fill in
base=(t>>18==15? "F       F       F   F   F F F   ":base),
basee=(t>>18==15?"20000000200000002000200020202000":basee),
basemod=(t>>18==15?32:(t>>20==4)?16:8),//Different pattern length depending on place in song.


snaree=(t>>20==3)?"00008880":(t>>20==4)||(t>>20==5)?"00002000":"00200020",
hihate=((t>>20==2)?"0000101010001000000001010100010000":(t>>20==3)?"10000000000010000000000000000000":(t>>20==4)?"0":"10001000100010001000100010001000"),


/*
===========
INSTRUMENTS
===========
*/

//Basic melody 1 (and 3 for intermezzo)
	((t>>20!=3)&&((t>>19!=14)&&(t>>19!=15))?tri(m,13,8,10,me, 0x1000, 13, 8, .18):0)
+//Bass 1
	((!(t>>20)||(t>>19==5))?pulse(b1m,13,64,7,b1e, 0x1000, 13,8, .2):0)
+//Bass 2 ( and 3 for intermezzo)
	((t>>20!=3)&&(t>>20!=6)&&(t>>20!=7)?saw(b2m,15,16,6,b2e, 0x2000, 13, 2, .3):0)
+//Bass 3
	((t>>20==3)||(t>>20==4)||(t>>20==5)?saw(b3m,17,8,6,b3e, 0x1000, 13,1, .15):0)
+//Bells 1
	((t>>20==1)||(t>>20==5)?tri(bell1m,13,64,9,bell1e, 0x300, 13,64, .15):0)
+//Bell 2
	((t>>20)==2?tri(bell2m,16,4,9,bell2e, 0x400, 13,8, .15):0)
+//Jump effect
	((t>>18==11)?jump(j,jb, 0x2000, 13,32,8,je, 0x1000, 13, 1, .2):0)

+//Melody 2
	((t>>20)==2?tri(m2,13,8,13,m2e, 0x1000, 13,8, .1):0)
+//Melody 3, intermezzo
	((t>>20==3||(t>>20==4))?saw(m3,13,8,4,m3e, 0x1000, 13, 8, .08):0)

+//Melody 4
	((t>>20)==3?tri(m4,16,8,9,m4e, 0x1000, 13,1, .1):0)
+//Melody 5 
	((t>>19==12)||(t>>19==13)?tri(m5,13,64,9,m5e, 0x1000, 13,8, .3):0)
+//Bass 4 (outro)
	((t>>19==14)||(t>>19==15)?saw(b4m,17,4,7,b4e, 0x1000, 13,16, .2):0)
+//Melody 6 (outro)
	((t>>19==14)||(t>>19==15)?saw(m6,13,64,4,m6e, 0x1000, 13,64, .2):0)
+
	((t>>20==6)?jump(j2,j2b, 0x2000, 14,1,8,j2e, 0x1000, 13, 8, .2):0)

+//Melody 7 (outro)
	((t>>19==14)||(t>>19==15)?tri(m7,12,4,9,m7e, 0x1000, 13,1, .1):0)
+//Melody 8 (outro)
	((t>>19==14)||(t>>19==15)?tri(m8,16,8,8,m8e, 0x1000, 12,4, .3):0)
+


//Hihat
	(((t>>19!=14)&&(t>>19!=15))?

	noise( 1, hihate, 0x800, 11, 32, .1)
+//Snare
	noise( 3, snaree, 0x1000, 13, 8, .2)

+//Bass Drum

	rkick(9,basee, 0x1000, 13,basemod, .3):0)
```
---
# Functions (Floatbeat)

## Small Floatbeat (< 256 Byte)

Name: Funcbeat example\
Creator: SArpnt\
From: https://github.com/SArpnt/bytebeat-composer

```js
let lastSample = 0; // variables here are static and can always be accessed
let resonanceMomentum = 0;
const notedata = "$$$000,,,,,,,,''"; // this is a good place to store constants like note values because creating arrays and such constantly ruins performance
// note that this is a bad example, a string like this isn't an issue
// this is also a good place to do decompression calculations, for storing things like samples efficiently

return function (time, sampleRate) { // time is in secs, note that samplerate can still change the sound when static variables are used, this is why samplerate is given
	const pitch = 2 ** ((notedata.charCodeAt(time * 4.3 & 15) + 22) / 12); // grab values from string and convert semitones to hz
	const pulse = ((time * pitch % 1 > (time / 2 % 1) * .6 + .2) - .5) / 2; // generate pulse wave
	lastSample += resonanceMomentum += (pulse - lastSample - resonanceMomentum * 3) / (cos(time / 5) * 170 + 200); // lowpass with resonance, doesn't work on other samplerates
	const kick = (sin((time * 4.3 % 2 + .01) ** .3 * 180)) / 4;
	return lastSample + kick;
}
```
---
# Infix (Signed Bytebeat)

## Small signed Bytebeat (< 256 Byte)

Name: 1fccccf1\
Creator: PortablePorcelain\
From: https://dollchan.net/btb/res/3.html#55

```js
a=((t*(Math.pow(2, (t>>16&3)-1)))*((0x1fccccf1>>(t>>10)*(t>>11))%8))|t>>3,b=a%128-32,c=sin(5000/(t&4095))*32,b+c
```

---

Name: remix of crude sinewave dubstep\
Creator: PortablePorcelain\
From: https://dollchan.net/btb/res/3.html#82

```js
c=(v)=>sin(v*PI),b=t/4,q=c(b*(b&b>>12)/11025/2)/2,n=((6e5/(b&16383))/256)%1,k=((6e5/(b+16383&32767))/256)%1.5,o=((6e4/(b&4095))/256)%0.5,g=min((4*b/[8,8,8,9,10,10,9,9][b>>15&7]&255)*(-b>>5&255)>>8,128),w=min((q+n-k-o)*96+g,127)
```

---

Name: No Limit another take on simplifying mu6k's thing\
Creator: ryg\
From: https://www.pouet.net/topic.php?which=8357&page=10#c388986

```js
sin(2000/(y=t&4095))*25+((x=t*(15&0x9866>>(t>>12&12))/6)&127)*(y/1e+4)+((t>>6^t>>8|t>>12|x)&63)
```

---

Name: funky remix of "the time is running out!"\
Creator:  getDolphinedLol\
From: https://www.reddit.com/r/bytebeat/comments/ry4ikf/comment/hrmh2m3/?utm_source=reddit&utm_medium=web2x&context=3

```js
(t>>4|t*(t&16384?7:5)*(3-(3&t>>9)+(3&t>>8))>>(3&-t>>(t%65536<59392?(t&4096?2:16):2))&96)+(64*random(t)*(1&t>>11))+(44444/(t%pow(4,(3+(2+(1)))))&128)
```

---

Name: No Limit\
Creator: mu6k\
From: http://www.pouet.net/topic.php?which=8357&page=9#c388931

```js
((sin(2000/(t&0xfff))*127)*0.2 + (( (t<<1)*(1+0.333*((t&0xffff)>0x7fff)+0.177*((t&0xffff)>0xbfff )) )&0xff)*((t&0xfff)/0x1fff)*0.4 + (( ( (t>>4^t>>6|t>>10))|(t*3*(1+0.333*((t&0xffff)>0x7fff)+0.177*((t&0xffff)>0xbfff))) )&0xff)*0.25)
```
---
## Medium signed Bytebeat (< 1 Kilobyte)

Name: Longline Theory\
Creator: mu6k\
From: http://www.pouet.net/topic.php?which=8357&page=13#c389287

```js
intro = (sb = t > 0xffff) & 0,
background =
	((y = pow(2, [15, 15, 23, 8][t >> 14 & 3] / 12)) & 0) + (
		((y * t * 0.241) & 127 - 64) +
		((y * t * 0.25) & 127 - 64)
	) * 1.2,
drum = (
	((a = 1 - (t & 0x7ff) / 0x7ff) & 0) +
	(((5 * t & 0x7ff) * a) & 255 - 127) *
	((0x53232323 >> (t >> 11 & 31)) & 1) * a * 1.0 +
	(((d = (14 * t * t ^ t) & 0x7ff) * a) & 255 - 128) *
	((0xa444c444 >> (t >> 11 & 31)) & 1) * a * 1.5 +
	((a * a * d * (t >> 9 & 1) & 0xff - 0x80) * 0.1337)
) * sb,
instrument = +((g = (t & 0x7ff) / 0x7ff) & 0) +
	((g = 1 - (g * g)) & 0) +
	((h = pow(2, [
		[15, 18, 17, 17, 17, 17, 999, 999, 22, 22, 999, 18, 999, 15, 20, 22],
		[20, 18, 17, 17, 10, 10, 999, 999, 20, 22, 20, 18, 17, 18, 17, 10]
	][((t >> 14 & 3) > 2) & 1][t >> 10 & 15] / 12)) & 0) +
	((h * t & 31) + (h * t * 1.992 & 31) + (h * t * 0.497 & 31) + (h * t * 0.977 & 31) - 64) *
g * 2.0 * sb,
intro + max(min(instrument + background + drum, 127), -128);
```

---

Name: Longline Theory 300b Information Theory\
Creators: ryg, las, decipher, p01\
From: http://www.pouet.net/topic.php?which=8357&page=17#c389541

```js
w=t>>9,k=32,m=2048,a=1-t/m%1,d=(14*t*t^t)%m*a,y=[3,3,4.7,2][p=w/k&3]*t/4,h="IQNNNN!!]]!Q!IW]WQNN??!!W]WQNNN?".charCodeAt(w/2&15|p/3<<4)/33*t-t,s=y*.98%80+y%80+(w>>7&&a*((5*t%m*a&128)*(0x53232323>>w/4&1)+(d&127)*(0xa444c444>>w/4&1)*1.5+(d*w&1)+(h%k+h*1.99%k+h*.49%k+h*.97%k-64)*(4-a-a))),s*s>>14?127:s
```

---

Name: weirdo boy\
Creator: Romash\
From: https://battleofthebits.org/arena/Entry/weirdo+boy/31403/

```js
freqs=[220,261,174*2,207],t2=t/11015*24,freq2=1+(t>>15&1)+(((41&(t>>12))%4)^(1+((t>>11)&2)-1)*((t>>10)>1024)),(((t2*(1-freq2)*0.999*freqs[t>>17&3])%128)+(((t2+500)*freq2*1.01*freqs[t>>17&3])%128)-128)/4+(((t2/2*0.999*freqs[t>>17&3])%128)+(((t2/2+500)*1.01*freqs[t>>17&3])%128)-128)*((t>>10)>1024)/4
```

