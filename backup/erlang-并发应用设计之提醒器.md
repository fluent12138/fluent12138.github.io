# [Erlang并发应用设计之提醒器](https://git.acwing.com/fluentx/erlang-reminder/-/tree/main/) 



> 主要文件为: evserv.erl(事件服务器)、event.erl(事件进程)、test.erl(测试，模拟客户端)、sup.erl(重启器)

## 理解问题
为了编写这个小应用， 需要做的事情

- 添加一个事件， 包括事件名，事件描述信息，最后期限
- 当事件到达约定期限， 发出警告
- 根据事件名字取消事件
- 通过命令行或系统进行交互



> 此应用没有做信息持久化， 通过Erlang数据结构在运行时存储信息

 
  **程序结构如下** 
![reminder-client-server.png](https://cdn.acwing.com/media/article/image/2023/03/26/36510_7e46851ecb-reminder-client-server.png) 

### server(以下统称事件服务器)需要做的任务
- 接收客户端的订阅
- 把来自事件服务器的消息转发给每个订阅者
- 接收增加事件的消息(需要时会启动x, y, z进程)
- 接收取消事件消息, 随后杀死事件进程

### 客户进程任务
- 向事件服务器发起订阅, 并接受通知信息
- 请求服务器增加一个具体的事件
- 请求服务器取消一个事件
- 监控服务器
- 在需要时, 关闭事件服务器

### 进程x,y,z需要做的任务
- 当计时器到时, 给事件服务器进程发送一条信息
- 接收事件取消消息, 然后死亡

![骨架.png](https://cdn.acwing.com/media/article/image/2023/03/26/36510_89eed0aacb-骨架.png)


> 在真实的应用中，把每个待提醒的事件都表示为一个进程的做法可能有些过度了，并且难以
扩展到大量事件的场合。不过，因为这个应用只有我一个用户，所以这种设计没有问题。

## 设计协议

### 客户端 <-> 服务器
  **订阅**
  
  - 客户端 -> 服务器
  - 客户端请求: {subscribe, self()}
  - 服务端响应: ok
  
  **添加** 

- 客户端 -> 服务器
- 客户端请求: {add, Name, Description, TimeOut}
- 服务端响应: ok|{error, reason}, 在TimeOut格式不正确会提示错误
 
  **取消** 

- 客户端 -> 服务器
- 客户端请求: {cancel, Name}
- 服务端响应: ok
  **事件完成** 
    
- 服务端 -> 客户端
- 服务器响应 : {done, Name, Description}

  **关闭** 
  
- 客户端 -> 服务器
- 客户端请求: shutdown
- 服务端响应: {'DOWN', Ref, process, Pid, shutdown}

### 事件服务器 <-> 事件

 **事件完成**
 
- 事件进程 -> 事件服务器进程
- 事件进程 : {done, Id}

 **取消事件** 
 
- 事件服务器进程 -> 事件进程
- 事件服务器进程 : {cancel}
- 事件进程: ok

### 升级服务器

- erlang shell -> 事件服务器
- erlang shel : {code_change}

## 具体实现

 **由于初学erlang, 项目文件可能有点混乱 hh** 
 
  [erlang-reminder](https://git.acwing.com/fluentx/erlang-reminder/-/tree/main/)

<!-- ##{"timestamp":1679839200}## -->