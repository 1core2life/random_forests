// 7.17 화요일

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

//데이터 라인 개수
#define MAX_DATA_NUM 220
//분할된 시퀀스 개수
#define MAX_DATA_INDEX 10
//클래스별 데이터 개수
#define MAX_CLASS_DATA 2
//전체 클래스 개수
#define CLASS_NUM 2
//나무 개수
#define TREE_NUM 5
//최대 데이터 값 크기
#define MAX_VALUE_SIZE 1000
//테스트 입력 데이터 라인 개수
#define TEST_MAX_DATA_NUM 30



enum CLASS { ROCK = 0 , SICCER };

struct Node {
    struct Node* left;
    struct Node* right;
    int target;
    double standard;
    enum CLASS retClass;
};

struct Standards{
    int index;
    int val;
};

struct Data{
    int** input;
    int data_length;
    enum CLASS* label;
    int index_length;
};

//---------------------
int testClassification(int test[], struct Node root);
struct Data createRandomData(struct Data data, int* random);
struct Data createTestData(void);
int testClassification_rf(int* input_data, struct Node root[TREE_NUM] , int random[TREE_NUM][MAX_DATA_INDEX / 2] );
void test_print(struct Data dat);
void print_labels(struct Data data);
//---------------------


double getProb(int data, int index, struct Data dat){
    double prob = 0;

    for(int i = 0 ; i< dat.data_length ; i ++){
        if(data == dat.input[i][index])
            prob++;
    }

    prob /= dat.data_length;
    return prob;
}

double shannonEntropy(){
    double shannon = 0;
    for(int i = 0; i < CLASS_NUM ; i++){
        shannon += (double)MAX_CLASS_DATA/MAX_DATA_NUM * log2((double)MAX_CLASS_DATA/MAX_DATA_NUM);
    }
    shannon *= -1;

    return shannon;
}

double getProbByAttr(int data , int index, struct Data dat){

    double prob = 0;
    double arrByClass[CLASS_NUM] = {0,};
    double tempProb = 0;
    int len = 0;

    for(int k = 0 ; k < dat.data_length ; k ++){
        if(data == dat.input[k][index]){
            arrByClass[dat.label[k]]++;
            len ++;
        }
    }

    for(int k = 0 ; k < CLASS_NUM ; k ++){
        arrByClass[k] /= len;
        if(arrByClass[k] != 0)
            tempProb += arrByClass[k] * log2(arrByClass[k]);
    }

    prob += getProb(data,index,dat) * tempProb;

    return prob;
}


struct Standards getBestStandard(struct Data dat){
    
    //===================== test print
    //test_print(dat);
    //===================== test print
    
    double h = shannonEntropy();

    int bestGainIndex = 0;
    double bestGainProb = 0;
    int nowIndex = 0;
    double prob = 0;

    for(int i = 0 ; i < dat.index_length ; i++){
        int alreadyDataBox[MAX_VALUE_SIZE] = { 0,};

        for(int k = 0 ; k < dat.data_length ; k++){
            int data = dat.input[k][i];

            if(alreadyDataBox[data] == 1)
                continue;

            prob += getProbByAttr(data,i,dat) ; // 정보이득 계산

            alreadyDataBox[data] = 1;
        }
        prob = h + prob;
        
        nowIndex = i ;
        
        if(prob >= bestGainProb){
            bestGainProb = prob;
            bestGainIndex = nowIndex;
        }

        prob = 0;
    }
    
    double bestProbInAttr = 0;
    int bestStandardVal = 0;
    
    for(int i = 0; i< dat.data_length ; i++){
        double tempProb = 0;
        
        int lenLow = 0;
        int lenHigh = 0;
        int data = dat.input[i][bestGainIndex];
        
        double arrByClassHigh[CLASS_NUM] = {0,};
        double arrByClassLow[CLASS_NUM] = {0,};
        
        for(int k = 0; k< dat.data_length ; k++){
            if(data < dat.input[k][bestGainIndex]){
                arrByClassHigh[dat.label[k]]++;
                lenHigh ++;
            }
            else{
                arrByClassLow[dat.label[k]]++;
                lenLow ++;
            }
        }
        
        if(lenHigh == 0 || lenLow == 0)
            continue;
        
        int alreadyDataBoxHigh[MAX_VALUE_SIZE] = { 0,};
        int alreadyDataBoxLow[MAX_VALUE_SIZE] = { 0,};

        for(int k = 0 ; k < dat.data_length ; k ++){
                if( data < dat.input[k][bestGainIndex]){
                    arrByClassHigh[dat.label[k]] /= lenHigh;
                    
                    double pro = arrByClassHigh[dat.label[k]];

                    if(arrByClassHigh[dat.label[k]] != 0 && alreadyDataBoxHigh[dat.label[k]] != 1)
                        tempProb += ((double)lenHigh/(double)dat.data_length) * pro * log2(pro);
                    alreadyDataBoxHigh[dat.label[k]] = 1;
                }
                else{
                    arrByClassLow[dat.label[k]] /= lenLow;
                    
                    double pro = arrByClassLow[dat.label[k]];

                    if(arrByClassLow[dat.label[k]] != 0 && alreadyDataBoxLow[dat.label[k]] != 1)
                        tempProb +=((double)lenLow/(double)dat.data_length) * pro* log2(pro);
                    alreadyDataBoxLow[dat.label[k]] = 1;
                }
        }

        tempProb *= -1;
        if(bestProbInAttr <= tempProb){
            bestProbInAttr = tempProb;
            bestStandardVal = data;
        }
    }

    struct Standards ret;
    ret.index = bestGainIndex;
    ret.val = bestStandardVal;

    return ret;
}


struct Data splitArray(struct Node* now,struct Data data, int flag){
    struct Data dat;

    int lessTemp[MAX_DATA_NUM][MAX_DATA_INDEX], upperTemp[MAX_DATA_NUM][MAX_DATA_INDEX];
    int lessLen = 0, upperLen = 0;
    int **less, **upper;

    int labelTemp[MAX_DATA_NUM];
    enum CLASS* label;

    //upper
    if(flag == 0){
        for(int i = 0; i< data.data_length ; i++){
            if(data.input[i][now->target] > now->standard){
                int idx = 0;
                for(int k = 0; k< data.index_length ; k++){
                    if(k != now->target)
                        upperTemp[upperLen][idx++] = data.input[i][k];
                }
                labelTemp[upperLen] = data.label[i];
                upperLen++;
            }
        }
        upper =(int**)malloc(sizeof(int*)*upperLen);
        for(int i =0 ; i< upperLen ; i++)
            upper[i] = (int*)malloc(sizeof(int)*( data.index_length - 1 ) );

        label = (enum CLASS*)malloc(sizeof(enum CLASS)*( upperLen ) );

        for(int i = 0; i < upperLen ; i++){
            for(int k = 0; k < data.index_length - 1  ; k++)
                upper[i][k] = upperTemp[i][k];
            label[i] = labelTemp[i];
        }

        dat.input = upper;
        dat.data_length = upperLen;
    }
    //less
    else{
        for(int i = 0; i< data.data_length ; i++){
            if(data.input[i][now->target] <= now->standard){
                int idx = 0;
                for(int k = 0; k< data.index_length ; k++){
                    if(k != now->target)
                        lessTemp[lessLen][idx++] = data.input[i][k];
                }
                labelTemp[lessLen] = data.label[i];
                lessLen++;
            }
        }

        less =(int**)malloc(sizeof(int*)*lessLen);
        for(int i =0 ; i< lessLen ; i++)
            less[i] = (int*)malloc(sizeof(int)*( data.index_length - 1 ) );

        label = (enum CLASS*)malloc(sizeof(enum CLASS)*( lessLen ) );

        for(int i = 0; i < lessLen ; i++){
            for(int k = 0; k < data.index_length - 1  ; k++)
                less[i][k] = lessTemp[i][k];
            label[i] = labelTemp[i];
        }
        dat.input =  less;
        dat.data_length = lessLen;
    }

    dat.label = label;
    dat.index_length = data.index_length - 1;

    return dat;
}

struct Data deleteOne(struct Data data){
    int** testData = (int**)malloc(sizeof(int*) * (data.data_length - 1) );
    for(int i =0 ; i< (data.data_length - 1) ; i++)
        testData[i] = (int*)malloc(sizeof(int) * (data.index_length - 1));
        
        for(int i =0 ; i< (data.data_length - 1) ; i++)
            for(int k =0 ; k< (data.index_length - 1) ; k++)
                testData[i][k] = data.input[i][k];
                
                enum CLASS* label = (enum CLASS*)malloc(sizeof(enum CLASS)* (data.data_length - 1) );
                for(int i =0 ; i< (data.data_length - 1)  ; i++)
                    label[i] = data.label[i];
                    
                    struct Data dat;
    dat.input = testData;
    dat.label = label;
    dat.data_length = (data.data_length - 1);
    dat.index_length = (data.index_length - 1);
    
    return data;
}


int* deleteOneIndex(int* data, int size,int idx){
    int* testData = (int*)malloc(sizeof(int) * size) ;
    
    int index = 0;
    
    for(int i = 0; i < size ; i++){
        if(i == idx)
            index++;
            
        testData[i] = data[index];
        index++;
    }
    
    return testData;
}

void devideData(struct Node* root, struct Data data){

    struct Node* now;

    for(int i = 0; i < data.data_length ; i++){
        now = root;
        int* row = data.input[i];
        while(now->left != NULL && now->right != NULL){
            
            int targetIndex = now->target;
            double standard = now->standard;
            
            
            if(row[targetIndex] > standard){
                now = now->right;
            }
            else{
                now = now->left;
            }
            
            row = deleteOneIndex(row , data.index_length -1 , targetIndex);

        }
        
        now->retClass = data.label[i];
        data = deleteOne(data);
    }

}

void print_labels(struct Data data){
    for(int i = 0; i< data.data_length ; i++)
        printf("%d ",data.label[i]);
    printf("끝 \n");
}


void createTree(struct Node* now, struct Data data, int depth ){

    if(data.data_length == 1 || data.index_length == 0)
        return ;
    
    struct Standards ret = getBestStandard(data);
    now->standard = ret.val;
    now->target = ret.index;
    now->left = NULL;
    now->right = NULL;
    
    struct Data upper = splitArray(now,data,0);
    struct Data less = splitArray(now,data,1);
    //print_labels(upper);
    //print_labels(less);

    if(*upper.input == NULL || *less.input == NULL)
        return ;
    
    if(*upper.input != NULL){
        struct Node* tp = malloc(sizeof(struct Node));
        now->right = tp;
        createTree(now->right, upper, depth + 1);
    }
    if(*less.input != NULL){
        struct Node* tp = malloc(sizeof(struct Node));
        now->left = tp;
        createTree(now->left, less, depth + 1);
    }
    
}

struct Data loadTrainingData(int flag){
    int** data;
    enum CLASS* label;
    FILE * fp;
    struct Data dd;
    
    //flag = 0, load training data
    //flag = 1, load test data
    if(flag == 0){
        data = (int**)malloc(sizeof(int*) * MAX_DATA_NUM);
        for(int i =0 ; i< MAX_DATA_NUM ; i++)
            data[i] = (int*)malloc(sizeof(int) * MAX_DATA_INDEX);
        label = (enum CLASS*)malloc(sizeof(enum CLASS)* MAX_DATA_NUM);
        fp =fopen("/Users/baegjiwon/Documents/machine_learning/machine_learning/Heart.csv","rt");
        
        dd.data_length = MAX_DATA_NUM;
    }
    else{
        data = (int**)malloc(sizeof(int*) * TEST_MAX_DATA_NUM);
        for(int i =0 ; i< TEST_MAX_DATA_NUM ; i++)
            data[i] = (int*)malloc(sizeof(int) * MAX_DATA_INDEX);
        label = (enum CLASS*)malloc(sizeof(enum CLASS)* TEST_MAX_DATA_NUM);
        fp =fopen("/Users/baegjiwon/Documents/machine_learning/machine_learning/test_Heart.csv","rt");
        
        dd.data_length = TEST_MAX_DATA_NUM;
    }

    int row = 0, col = 0 , labelIdx = 0;
    
    char ch;
    char word[256];
    int idx = 0;
    
    while(1){
        if( (row == MAX_DATA_NUM && flag == 0) || (row == TEST_MAX_DATA_NUM && flag == 1) )
            break;
        
        ch=fgetc(fp);
        if(ch == ','){
            data[row][col++] = atoi(word);
            for(int i = 0; i< idx ; i++)
                word[i] = NULL;
            
            idx = 0;
            continue;
        }
        if(ch == '\n' || ch == EOF){
            col = 0;
            row ++;
            if( word[0] != 'N' )
                label[labelIdx ++ ] = 0;
            else
                label[labelIdx ++ ] = 1;
            
            for(int i = 0; i< idx ; i++)
                word[i] = NULL;
            
            idx = 0;
            if(ch == EOF)
                break;
        }
        word[idx++] = ch;
    }
    
    fclose(fp);

//    for(int i = 0; i < TEST_MAX_DATA_NUM ; i++){
//        for(int k = 0; k < MAX_DATA_INDEX ; k++){
//            printf("%d ",data[i][k]);
//        }
//        printf("\n");
//    }
//
//    for(int i = 0; i < TEST_MAX_DATA_NUM ; i++){
//
//        printf(" %d|%d ",i,label[i]);
//    }

    dd.input = data;
    dd.label = label;
    dd.index_length = MAX_DATA_INDEX;

    return dd;
}

int main() {
    srand(time(NULL));
    
    //입력할 학습 데이터
    struct Data data = loadTrainingData(0);
    double mean = 0;
    
    for(int k = 0; k< 10 ; k++){
        //학습 모델
        struct Node root[TREE_NUM];
        //랜덤포레스트 구현시 랜덤 인덱스
        int random[TREE_NUM][MAX_DATA_INDEX / 2];
        
        for(int i = 0; i< TREE_NUM ; i++){
            struct Data randData = createRandomData(data, random[i]);

            //트리 생성
            createTree(&root[i], randData, 0);

            //데이터 넣기로 class 분류
            devideData(&root[i],randData);
        }

        struct Data test_data = loadTrainingData(0);
        //int test_input_data[MAX_DATA_INDEX] = { 2 , 1 , 5 , 3 , 2 , 0};
        int correct = 0;
        for(int i = 0; i < TEST_MAX_DATA_NUM ; i++){
            int result = testClassification_rf(test_data.input[i], root,random);
            if(result == test_data.label[i]){
                correct++;
            }
        }
        mean +=(double)correct/TEST_MAX_DATA_NUM * 100;
        printf("정답률 : %f\n",(double)correct/TEST_MAX_DATA_NUM * 100);
    
    }
    printf("정답률 평균 : %f\n",mean/10);
    
    return 0;
}


struct Data createRandomData(struct Data data, int* random){
    
    for(int i = 0; i < MAX_DATA_INDEX / 2 ; i++){
        random[i] = rand() % MAX_DATA_INDEX;
    }
    
    int** testData = (int**)malloc(sizeof(int*) * data.data_length );
    for(int i =0 ; i< data.data_length ; i++)
        testData[i] = (int*)malloc(sizeof(int) * MAX_DATA_INDEX / 2 );
        
        
        for(int k = 0; k < MAX_DATA_INDEX / 2 ; k++){
            for(int i = 0; i < data.data_length ; i++){
                testData[i][k] = data.input[i][ random[k] ];
            }
        }
    
    struct Data dat;
    dat.data_length = data.data_length;
    dat.index_length = data.index_length / 2;
    dat.input = testData;
    dat.label = data.label;
    
    return dat;
}

struct Data createTestData(){

    int** testData = (int**)malloc(sizeof(int*) * MAX_DATA_NUM);
    for(int i =0 ; i< MAX_DATA_NUM ; i++)
        testData[i] = (int*)malloc(sizeof(int) * MAX_DATA_INDEX);

        int testData2[MAX_DATA_NUM][MAX_DATA_INDEX] ={  {1 , 2 , 1 , 1 , 0 , 5},{2 , 1 , 4 , 3 , 4 , 5}
            , {3 , 1 , 4 , 3 , 2 , 6}, {2 , 1 , 4 , 2 , 3 , 3}  };

        for(int i =0 ; i< MAX_DATA_NUM ; i++)
            for(int k =0 ; k< MAX_DATA_INDEX ; k++)
                testData[i][k] = testData2[i][k];

                enum CLASS label2[MAX_DATA_NUM] = { ROCK, SICCER, SICCER, ROCK };

                enum CLASS* label = (enum CLASS*)malloc(sizeof(enum CLASS)* MAX_DATA_NUM);
                for(int i =0 ; i< MAX_DATA_NUM ; i++)
                    label[i] = label2[i];

                    struct Data data;
    data.input = testData;
    data.label = label;
    data.data_length = MAX_DATA_NUM;
    data.index_length = MAX_DATA_INDEX;

    return data;
}



int* splitData(int* input, int random[MAX_DATA_INDEX / 2]){
    
    
    int* splitData = (int*)malloc(sizeof(int) * MAX_DATA_INDEX / 2 );
    for(int i = 0 ; i< MAX_DATA_INDEX / 2 ; i++){
        splitData[i] = input[random[i]];
    }
    
    return splitData;
}

//입력값 분류 모델
//입력 | input_data : 입력 EMG 센서 , root : 학습 모델, random : 학습 모델
//출력 | ret: 가장 큰 값을 갖는 int 값 반환
int testClassification_rf(int* input_data, struct Node root[TREE_NUM] , int random[TREE_NUM][MAX_DATA_INDEX / 2] ){

    int ret[CLASS_NUM] = {0, };
    struct Node* now;

    for(int i = 0; i< TREE_NUM ; i++){
        now = &root[i];
        int* splitedData = splitData(input_data, random[i]);

        while(now->left != NULL && now->right != NULL){
            int targetIndex = now->target;
            double standard = now->standard;

            if(splitedData[targetIndex] > standard){
                now = now->right;
            }
            else{
                now = now->left;
            }
        }
        ret[now->retClass] ++;
    }

    int best = 0;
    int bestIndex = 0;
    for(int i = 0 ; i < CLASS_NUM  ;i++){
        if(ret[i] >= best){
            best = ret[i];
            bestIndex = i;
        }
    }
    
    //ret 배열 내 가장 큰 값 반환
    return bestIndex;
}



void test_print(struct Data dat){
    for(int i = 0; i<dat.data_length ; i ++){
        for(int k = 0; k<dat.index_length ; k ++){
            printf("%d ",dat.input[i][k]);
        }
        printf("\n");
    }
    printf("\n");
    printf("\n");
}
