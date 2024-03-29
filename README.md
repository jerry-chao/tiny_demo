# 简答的IM的演示示例

## 基础概念

### 单聊

### 群聊

## 计划

### 协议制定

协议选择上使用protobuf的二进制协议

1.  协议说明

    | 字段    | 类型  | 说明                                       |
    |------- |----- |------------------------------------------ |
    | version | INT8  | 协议版本号                                 |
    | command | enum  | 顶层命令类型，CONNECT，DISCONNECT，REQUEST，RESPONSE |
    | payload | bytes | 不同的协议表示不同的含义                   |

    1.  登录 CONNECT

        | 字段   | 类型   | 说明     |
        |------ |------ |-------- |
        | org    | string | 组织     |
        | appkey | string | 部门     |
        | user   | string | 账号     |
        | token  | string | 密码/token |

    2.  断开 DISCONNECT

        | 字段   | 类型   | 说明 |
        |------ |------ |---- |
        | code   | INT8   | 断开码 |
        | reason | string | 断开原因 |

    3.  请求 REQUEST

        | 字段    | 类型   | 说明                           |
        |------- |------ |------------------------------ |
        | type    | ENUM   | 请求的具体类型， MESSAGE，CONTACT，GROUP |
        | payload | bytes  | 请求的具体内容，详解见分开解释 |
        | opts    | Option | 扩展信息列表                   |

        扩展信息

        | 字段    | 类型 | 说明   |
        |------- |---- |------ |
        | offline | bool | 是否保存离线 |

        1.  消息 MESSAGE

            | 字段    | 类型  | 说明             |
            |------- |----- |---------------- |
            | type    | ENUM  | 消息具体内容，TXT，IMAGE |
            | payload | bytes | 消息的具体内容   |

            1.  文本消息

                | 字段 | 类型   | 说明 |
                |--- |------ |---- |
                | txt | string | 文本内容 |

            2.  图片消息

                | 字段 | 类型   | 说明 |
                |--- |------ |---- |
                | url | string | 文本内容 |

    4.  相应 RESPONSE

        | 字段   | 类型   | 说明 |
        |------ |------ |---- |
        | code   | INT8   | 请求结果 |
        | reason | string | 结果说明 |

2.  protobuf如下所示

    ```protobuf
    syntax = "proto3";

    enum CommandType {
        CONNECT = 0;
        DISCONNECT = 1;
        REQUEST = 2;
        RESPONSE = 3;
    }

    message Command {
        int32 version = 1;
        CommandType command = 2;
        bytes payload = 3;
    }

    message Connect {
        string org = 1;
        string appkey = 2;
        string user = 3;
        string token = 4;
    }

    message Disconnect {
        int32 code = 1;
        string reason = 2;
    }

    enum RequestType {
        MESSAGE = 0;
        CONTACT = 1;
        GROUP = 2;
    }

    message Option {
        bool offline = 1;
    }

    message Request {
        RequestType type = 1;
        bytes payload = 2;
        repeated Option opts = 3;
    }

    enum MessageType {
        TXT = 0;
        IMAGE = 1;
    }

    message MessageTxt {
        string txt = 1;
    }

    message MessageImg {
        string url = 1;
    }

    message Message {
        MessageType type = 1;
        bytes payload = 2;
    }

    message Response {
        int32 code = 1;
        string reason = 2;
    }
    ```

3.  protobuf具体实现

### 长连接

### 短连接
