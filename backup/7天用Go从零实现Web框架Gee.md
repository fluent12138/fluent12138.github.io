## 准备阶段
- 环境搭建, 搭了两份
    - 使用wsl2 ubuntu镜像, 直接命令行安装, 嘎嘎快
    - windows本机安装go,  [参考链接](https://blog.csdn.net/qq_34902437/article/details/120699775) 
- ide
    - 熟悉语法阶段: `vim` (y总配置嘎嘎好用)
    - 正式编写阶段: `idea 2021`
- 版本控制: `git`

## day1
- 代码量: `50行`
- 主要内容
    - 了解go中http模块的作用
    - 自定义路由 + 回调方法
    - 框架结构定义

**框架基本结构**
```go
// 文件结构
.
├── base1
│   └── main.go
├── base2
│   └── main.go
└── base3
    ├── fgee
    │   ├── fgee.go
    │   └── go.mod
    ├── go.mod
    └── main.go

// 只有一个路由对应的map
// 比如定义了一个路由为 "/hello", 回调方法为helloFunc, 就会存在这个结构体中 {"/hello" : helloFunc}
// 当请求了这个路径, 通过路径找到对应方法, 然后执行

type Engine struct {
	router map[string]HandlerFunc
}
```

## day2

- 总代码量:约`140行`, 新增`90行`
- 主要内容
    - 封装数据
        - 定义上下文封装请求过程需要的数据
        - 封装路由为单独一个模块, `router.go`
    - 定义通用方法: 例如响应HTML页面, 响应字符串, 响应json数据

**框架基本结构**

```go
// 文件结构
.
├── fgee
│   ├── context.go
│   ├── fgee.go
│   ├── go.mod
│   └── router.go
├── go.mod
└── main.go

// fgee.go文件

type Engine struct {
	router *router // 由之前的map封装为结构体
}

// context.go文件
// Context 上下文结构
// 在ServeHTTP方法中被创建

type Context struct {
	// 请求与响应
	Req    *http.Request
	Writer http.ResponseWriter
	// 请求信息
	Path   string
	Method string
	// 响应信息
	StatusCode int
}

// 每次请求都会创建一个单独的context, 为这个请求服务
```

## day3

- 总代码量: 约`290行` 新增代码: 约`150行`
- 主要内容
    - 添加动态路由, 使用`字典树`
    - 支持 `:name`与`*filepath`两种模式
- 有点忘了字典树是啥了, 写之前还回顾了一下基础课字典树 : )

 **字典树操作** 
 
- 插入: 对应创建路由时, 把路由插入字典树中
- 查询: 当请求来临时, 去字典树找到对应的路由

**框架基本结构**
```go
// 文件结构
.
├── fgee
│   ├── context.go
│   ├── fgee.go
│   ├── go.mod
│   ├── router.go
│   ├── router_test.go
│   └── tire.go
├── go.mod
└── main.go

// context.go

// Context 上下文结构
// 在ServeHTTP方法中被创建
type Context struct {
	// 请求与响应
	Req    *http.Request
	Writer http.ResponseWriter
	// 请求信息
	Path   string
	Method string
	// 路由匹配后的参数
	Params map[string]string
	// 响应信息
	StatusCode int
}
```
- 存在问题: 字典树处理不严谨, 路由会冲突, 比如创建路由/:age, 再创建路由/16, 这样就会出现冲突问题


**教程链接**

> [7天用Go从零实现Web框架Gee教程](https://geektutu.com/post/gee.html)