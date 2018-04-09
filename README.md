## R Add In RDMA 설치방법 & 사용가이드
   
## R-Project Download

https://www.r-project.org/

![image](https://user-images.githubusercontent.com/36947676/38477309-d78b77c2-3bec-11e8-989f-f852a20bbadf.png)

- 일반적으로 한국-서울대 R-project 설치

![image](https://user-images.githubusercontent.com/36947676/38477414-5cc2f032-3bed-11e8-9361-04f41971c7bc.png)

- 해당 OS에 맞게 선택

![image](https://user-images.githubusercontent.com/36947676/38478854-60d96440-3bf6-11e8-90d4-63f8b3b60b4a.png)

- Base 선택

![image](https://user-images.githubusercontent.com/36947676/38480176-e9bc17b4-3bfe-11e8-8280-b7d100653646.png)

## R-Studio Download

https://www.rstudio.com/products/rstudio/download/

- 해당 OS에 맞게 선택

![image](https://user-images.githubusercontent.com/36947676/38480229-3ffa14f0-3bff-11e8-9d79-710b2a5d6b1d.png)

## R Package Install

- devtools (github 내의 Package install 시 필요)

![image](https://user-images.githubusercontent.com/36947676/38480605-7620d3b4-3c01-11e8-8dd2-256ce7fcb644.png)

- RDMA

library(devtools)  
install_github("gmlgml1230/RDMA")  
library(RDMA)  
![image](https://user-images.githubusercontent.com/36947676/38480763-6096133c-3c02-11e8-8f09-4ed8966e2144.png)

- RDMA Add in 확인

![image](https://user-images.githubusercontent.com/36947676/38480794-877a3316-3c02-11e8-9188-c3e0a7a43051.png)


## RDMA Omniture

- Omniture Login

![image](https://user-images.githubusercontent.com/36947676/38481107-1761cb28-3c04-11e8-97d4-8392f573779a.png)
![image](https://user-images.githubusercontent.com/36947676/38481966-43883bd4-3c08-11e8-9973-82a94a3812c4.png)
![image](https://user-images.githubusercontent.com/36947676/38481141-4018726a-3c04-11e8-85b9-0751fdab7aa9.png)

- Data Update (Metric & Element & Segment 값을 처음 한 번만 업데이트하면되며 업데이트가 필요할 때 해당 국가 선택 후 사용)

![image](https://user-images.githubusercontent.com/36947676/38481342-24ed94c4-3c05-11e8-9187-8dd82017d8a1.png)
![image](https://user-images.githubusercontent.com/36947676/38481186-77a8601e-3c04-11e8-8ffa-4129b99e9a4b.png)

- Data Extract (Segment 없을 시 생략 가능)

![image](https://user-images.githubusercontent.com/36947676/38481447-d331296a-3c05-11e8-8f0c-8ae678619117.png)
![image](https://user-images.githubusercontent.com/36947676/38481641-c128bf34-3c06-11e8-8bca-ba5ec3dd0955.png)

- 추출 데이터 확인 및 다운로드

![image](https://user-images.githubusercontent.com/36947676/38481669-ee1bc4fa-3c06-11e8-8f10-2fc3005a7d66.png)
![image](https://user-images.githubusercontent.com/36947676/38481694-0b20abec-3c07-11e8-936c-cd4560c34e98.png)
![image](https://user-images.githubusercontent.com/36947676/38482441-448ddb7c-3c0a-11e8-8438-b28bc535f205.png)

## RDMA Adwords

- Adwords 인증 (No : 클릭 시 인증창 활성화, OK : 클릭 시 반응 없음)

![image](https://user-images.githubusercontent.com/36947676/38482919-1916c95c-3c0c-11e8-962b-74ba90ff71f4.png)  
![image](https://user-images.githubusercontent.com/36947676/38482945-2c0b3688-3c0c-11e8-95c7-40575daed823.png)  
![image](https://user-images.githubusercontent.com/36947676/38485677-9ba56b9a-3c15-11e8-8241-8a4df4c2d761.png)  
![image](https://user-images.githubusercontent.com/36947676/38485715-bd69dc98-3c15-11e8-8982-496a74659034.png)  
![image](https://user-images.githubusercontent.com/36947676/38485744-d61f3d0a-3c15-11e8-8306-e6d8fbeacfaa.png)    
![image](https://user-images.githubusercontent.com/36947676/38485640-7bb082e8-3c15-11e8-9571-e2d2b0638538.png)  
![image](https://user-images.githubusercontent.com/36947676/38485770-f0e6def4-3c15-11e8-8083-eb373357aa75.png)  
  
- Date & Report Name & Metric Name & Client Customer ID 선택 후 Date Extract  
  
![image](https://user-images.githubusercontent.com/36947676/38486803-070e1d16-3c19-11e8-88b9-0168f144c132.png)  
![image](https://user-images.githubusercontent.com/36947676/38486814-14fccd28-3c19-11e8-8553-96dfb5a60753.png)  
  
- 추출 데이터 확인 및 다운로드  
  
![image](https://user-images.githubusercontent.com/36947676/38486868-41686408-3c19-11e8-9b03-454cb88ec24b.png)
