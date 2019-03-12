#include<iostream>
#include<io.h>
#include<vector>
#include<list>
#include<string.h>
#include<fstream>
#include <ctype.h>
#include <algorithm>
#include<math.h>
using namespace std;
vector<string>files;//存储邮件的地址
vector<string>hamvec;//词表
vector<string>spamvec;
vector<string>testvec;
vector<vector<string> >tt;
vector<int>dx;//被测试单词集向量
vector<int>dhamvec;//正常邮件单词集向量
vector<int>dspamvec;//垃圾邮件单词集向量
int hamnum=0;
int spamnum=0;
int testnum=0;
double num=0;
#define HAMPATH  "../email/ham/"
#define SPAMPATH "../email/spam/"
#define TESTPATH "../email/test/"
void getAllFiles(string path, vector<string>& files) {
    //文件句柄
    long hFile = 0;
    files.clear();
    //文件信息
    struct _finddata_t fileinfo;  //很少用的文件信息读取结构
    string p;  //string类很有意思的一个赋值函数:assign()，有很多重载版本
    if ((hFile = _findfirst(p.assign(path).append("\\*").c_str(),&fileinfo)) != -1) {
        do {
            if ((fileinfo.attrib & _A_SUBDIR)) {  //比较文件类型是否是文件夹
                 if (strcmp(fileinfo.name,".") != 0 && strcmp(fileinfo.name,"..") != 0) {
                     files.push_back(p.assign(path).append("\\").append(fileinfo.name));
                     getAllFiles(p.assign(path).append("\\").append(fileinfo.name), files);
                 }
             } else {
                 files.push_back(p.assign(path).append("\\").append(fileinfo.name));
             }         } while (_findnext(hFile, &fileinfo) == 0);  //寻找下一个，成功返回0，否则-1
         _findclose(hFile);
     }
 }
void filter(string s,vector<string> &svec)//过滤掉空格和标点符号，将单词存入svec
{
    s+=" ";
    string tmp="";
    for(int i=0;i<s.size();i++)
    {

        if(s[i]>='a'&&s[i]<='z'||s[i]>='A'&&s[i]<='Z')
        {
            tmp+=s[i];
        }
        if(tmp.size()>0&&s[i]==' '||s[i]==','||s[i]=='.'||s[i]=='!'||s[i]=='?')
        {
            if(tmp.size()>1)
            {
                svec.push_back(tmp);
            }
            tmp.clear();
        }
    }
}
 void reademail(vector<string>&svec,int flag)
 {
     if(flag==2)
     {
         spamnum=files.size();
         //cout<<"读取垃圾邮件,共有:"<<spamnum<<endl;
     }
     if(flag==1)
     {
         hamnum=files.size();
         //cout<<"读取正常邮件,共有"<<hamnum<<endl;
     }
     if(flag==0)
     {
         testnum=files.size();
     }
     for(int i=0;i<files.size();i++)
     {
         string s;
         fstream f(files[i]);
         if(!f.is_open())
         {
             cout<<"邮件读取失败!"<<endl;
             exit(1);
         }
         while(getline(f,s))
         {
             //cout<<s<<endl;
             filter(s,svec);
         }
         f.close();
     }

 }
 void buildwords(string Path,vector<string>&WordsName,int flag)//flag>1表示正常邮件和垃圾有邮件
 {
    getAllFiles(Path, files);
    reademail(WordsName,flag);
    sort(WordsName.begin(),WordsName.end());
    //正常邮件和垃圾邮件的词汇表要去除重复元素，被测试的邮件则不需要,
    if(flag>0)
    {
        WordsName.erase(unique(WordsName.begin(),WordsName.end()),WordsName.end());
        //cout<<WordsName.size()<<endl;
    }

 }
 bool IsExist(string s,vector<string>svec)
 {

     for(int i=0;i<svec.size();i++)
     {
         if(s==svec[i])
            return true;
     }
     return false;
 }
void BuildVectorTable(vector<string>svec,vector<string>te,vector<int>&dvec)
{
    //svec是正常邮件单词集,te是被测试邮件单词集,devc用来存储生成的向量
//    cout<<"正常邮件单词集:"<<endl;
//    for(int i=0;i<svec.size();i++)
//    {
//        cout<<svec[i]<<" ";
//    }
//    cout<<"\n被测试邮件单词集:\n";
//    for(int i=0;i<testvec.size();i++)
//    {
//        cout<<testvec[i]<<" ";
//    }
    dvec.clear();
    for(int i=0;i<svec.size();i++)
    {
        if(IsExist(svec[i],te))
            dvec.push_back(1);
        else
            dvec.push_back(0);
    }
}
double CosineSimilarityCalculation(vector<int>ivec1,vector<int>ivec2)
{
    double absolute_a=0,absolute_b=0,product_ab=0;
    if(ivec1.size()!=ivec2.size())
    {
        cout<<"余弦相似度计算出错!";
        exit(1);
    }
    for(int i=0;i<ivec1.size();i++)
    {
        absolute_a+=ivec1[i]*ivec1[i];
        absolute_b+=ivec2[i]*ivec2[i];
        product_ab+=ivec1[i]*ivec2[i];
    }
    if(absolute_a==0||absolute_b==0)
        return 0;
    //cout<<"a*b:"<<product_ab<<endl;
    //cout<<"||a||:"<<absolute_a<<" ||b||:"<<absolute_b<<endl;
    double cos_ans=product_ab/(sqrt(absolute_a)*sqrt(absolute_b));
    return cos_ans;
}
void Bayes()
{
    /*p(c|x)=p(x|c)*p(c)/p(x),其中px=p(x|c1)+p(x|c2)，c1是代表正常邮件
    ，c2代表垃圾邮件首先算出p(x|c),即dhamvec与(1......1)做余弦运算*/
    //进行余弦运算
    vector<int>globe;
    globe.clear();
    for(int i=0;i<dhamvec.size();i++)
    {
        globe.push_back(1);//构造全局向量
    }
    double px=0,pcx0=0,pcx1=0;
    //cout<<"正常邮件:"<<hamnum<<",垃圾邮件:"<<spamnum<<";共有邮件"<<hamnum+spamnum<<endl;
    double pham=hamnum/(double)(hamnum+spamnum);
    //cout<<"正常邮件的先验概率:"<<pham<<endl;
    double pspam=spamnum/(double)(hamnum+spamnum);
    //cout<<"垃圾邮件的先验概率:"<<pspam<<endl;
    pcx0=CosineSimilarityCalculation(dhamvec,globe);
    //cout<<"pxc0:"<<pxc0<<endl;
    globe.clear();
    for(int i=0;i<dspamvec.size();i++)
        globe.push_back(1);
    pcx1=CosineSimilarityCalculation(dspamvec,globe);
    //cout<<"pxc1:"<<pxc1<<endl;
    double ham_ans,spam_ans;
    if(pcx0==0)
    {
        //被测邮件与正常邮件的相似度为0，直接判定其为垃圾邮件
        ham_ans=0;
        spam_ans=1;
    }
    else if(pcx1==0)
    {
        //被测邮件与垃圾邮件的相似度为0，直接判定其为正常邮件
        ham_ans=1;
        spam_ans=0;
    }
    else
    {
        px=pcx0+pcx1;
        ham_ans=pcx0*pham/px;
        spam_ans=pcx1*pspam/px;
    }
    cout<<"被测邮件是正常邮件的概率是:"<<ham_ans<<endl;
    cout<<"被测邮件是垃圾邮件的概率是:"<<spam_ans<<endl;
    if(ham_ans>=spam_ans)
    {
        cout<<"该邮件是正常邮件!"<<endl;
    }
    else
    {

        cout<<"该邮件是垃圾邮件!"<<endl;
        num++;
    }
}
void ReadTestWords(string path)
{
    vector<string>spath;
    vector<string>stmp;
    getAllFiles(path,spath);
    string s;
    for(int i=0;i<spath.size();i++)
    {
        //cout<<"第"<<i+1<<"封邮件!"<<endl;
        //cout<<spath[i]<<endl;
        fstream f(spath[i]);
        if(!f.is_open())
         {
             cout<<"邮件读取失败!"<<endl;
             exit(1);
         }
         while(getline(f,s))
         {
             filter(s,stmp);
         }
         tt.push_back(stmp);
         stmp.clear();
    }
}
int main()
{
    //建立正常邮件的词汇表
    buildwords(HAMPATH,hamvec,1);
    //建立垃圾邮件的词汇表
    buildwords(SPAMPATH,spamvec,2);
    //建立被测试邮件的词汇表
    ReadTestWords(TESTPATH);
//    for(int i=0;i<testvec.size();i++)
//    {
//        cout<<testvec[i]<<" ";
//    }
    /*接下来根据北侧单词集里的单词是否在单词表中出现构造向量，出现为1，没出现为0
    公式:p(c|x)=p(x|c)*p(c)/p(x),其中p(x)展开成全概率公式来计算*/
    cout<<"正常邮件集:"<<hamnum<<"封，垃圾邮件集:"<<spamnum<<endl;
    for(int i=0;i<tt.size();i++)
    {
        BuildVectorTable(hamvec,tt[i],dhamvec);
        BuildVectorTable(spamvec,tt[i],dspamvec);
        //对每封邮件用贝叶斯算法分类
        Bayes();
    }
    cout<<"共判定:"<<tt.size()<<"封;误判:"<<num<<"封;"<<"准确率:"<<(tt.size()-num)/tt.size()<<endl;


}
