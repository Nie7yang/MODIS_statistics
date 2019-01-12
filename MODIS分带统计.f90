    program MODIS_FenDaiTongJi
    implicit none

    integer i,j,M,hang,lie,N,Mianji,k,Time
    character kong
    character(2) hao
    integer,allocatable:: DEM(:,:),PoDu(:,:),PoXiang(:,:),JiXue(:,:),JieDian(:)
    real(8),allocatable:: Gaocheng(:),DaiMian(:)
    reaL(8) Du(5),Xiang(9),DuMian(5),XiangMian(9)


    open(111,file='高程.txt')
    open(222,file='坡度.txt')
    open(333,file='坡向.txt')

    !open(555,file='结果.csv')

    write(*,*)'******** 输入你的处理数量：      ********'
    read(*,*)Time


    read(111,*)kong,lie
    read(111,*)kong,hang
    read(111,*)
    read(111,*)
    read(111,*)
    read(111,*)
    DO i=1,6
        read(222,*)
        read(333,*)
    ENDDO

    write(*,*)'******** 输入你的高程带数量：      ********'
    read(*,*)N

    allocate (DEM(hang,lie),PoDu(hang,lie),PoXiang(hang,lie),JiXue(hang,lie),Gaocheng(N),JieDian(N),DaiMian(N))

    DO i=1,N
        write(*,*)'******** 输入第',i,'一个断点高程：      ********'
        read(*,*)JieDian(i)
    ENDDO

    do i=1,hang
        read(111,*) (DEM(i,j),J=1,lie)
        read(222,*) (PoDu(i,j),J=1,lie)
        read(333,*) (PoXiang(i,j),J=1,lie)
    enddo

    DO M=1,Time
        write(Hao,'(I2)')M

        open(444,file=Trim(Hao))
        open(555,file='结果'//Trim(Hao)//'.csv')
        DO i=1,6
            read(444,*)
        ENDDO
        DO i=1,hang
            read(444,*)(JiXue(i,j),J=1,lie)
        ENDDO

        MianJi=0
        GaoCheng=0.0d0
        Du=0.0d0
        Xiang=0.0d0
        DaiMian=0.0d0
        DuMian=0.0d0
        XiangMian=0.0d0

        DO j=1,lie
            DO i=1,hang
                if(dem(i,j)>0)then
                    MianJi=Mianji+1
                    DO k=1,N-1
                        if(dem(i,j)>=JieDian(k).and.dem(i,j)<JieDian(k+1))then
                            DaiMian(k)=DaiMian(k)+1.0d0
                            exit
                        endif
                    ENDDO
                    if(dem(i,j)>=JieDian(N))then
                        DaiMian(N)=DaiMian(N)+1.0d0
                    endif

                    DO k=1,5
                        if(PoDu(i,j)==K)then
                            DuMian(k)=DuMian(k)+1.0d0
                            exit
                        endif
                    ENDDO

                    DO k=1,9
                        if(PoXiang(i,j)==k)then
                            XiangMian(k)=XiangMian(k)+1.0d0
                            exit
                        endif
                    ENDDO

                    if(JiXue(i,j)==200)then
                        DO k=1,N-1
                            if(dem(i,j)>=JieDian(k).and.dem(i,j)<JieDian(k+1))then
                                Gaocheng(k)=GaoCheng(k)+1.0d0
                                exit
                            endif
                        ENDDO
                        if(dem(i,j)>=JieDian(N))then
                            GaoCheng(N)=GaoCheng(N)+1.0d0
                        endif
                        DO k=1,5
                            if(PoDu(i,j)==K)then
                                Du(k)=Du(k)+1.0d0
                                exit
                            endif
                        ENDDO
                        DO k=1,9
                            if(PoXiang(i,j)==k)then
                                Xiang(k)=Xiang(k)+1.0d0
                                exit
                            endif
                        ENDDO
                    endif
                endif
            ENDDO
        ENDDO

        GaoCheng=GaoCheng*100.0d0/DaiMian
        Du=Du*100.0d0/DuMian
        Xiang=Xiang*100.0d0/XiangMian
        DO i=1,N
            write(555,*) i,',',GaoCheng(i)
        ENDDO
        write(555,*)
        write(555,*)
        DO i=1,5
            write(555,*) i,',',Du(i)
        ENDDO
        write(555,*)
        write(555,*)
        DO i=1,9
            write(555,*) i,',',Xiang(i)
        ENDDo
        close(444)
        close(555)
    ENDDO

    DEALLOCATE(DEM,PoDu,PoXiang,JiXue,GaoCheng,DaiMian)
    close(111)
    close(222)
    close(333)
    end program MODIS_FenDaiTongJi