Red [
    Title: "生成时序图"
    Author: "mahengyang"
    File: %main.red
    Needs: 'View
]
edge: 20
flow-gap: 30
default-line-height: 24 ;行高
default-font-size: 20 ;字体大小
font-width: 12
font-height: 12
default-font: make font! [size: default-font-size name: "Consolas" style: 'bold]
small-font: make font! [size: 9 name: "Consolas"]

data: [
    ["a" "b" "请求b"]
    ["b" "c" "b请求c"]
    ["c" "b" "c响应b"]
    ["b" "c" "b请求c 又一次"]
    ["c" "d" "c请求d"]
    ["d" "c" "d响应c"]
    ["c" "b" "c响应b 又一次"]
    ["b" "a" "b响应a"]
    ["a" "c" "a请求c"]
    ["c" "a" "c响应a"]
]
node-width: 60
node-height: 30
node-gap: 100
vline-height: 400
nodes-x: #()

draw-arrow: function ["绘制虚线" source dest y note][
    append my-draw compose [line-width 1]
    part-length: 8
    gap: 2
    arrow-offset: 5
    source-x: select nodes-x source
    dest-x: select nodes-x dest
    either source-x > dest-x [
        line-start: dest-x + arrow-offset 
        line-end: source-x - arrow-offset
        up-arrow-start: as-pair (line-start + arrow-offset) y + 3
        down-arrow-start: as-pair (line-start + arrow-offset) y - 3
        arrow-vertex: as-pair line-start y ; 箭头的顶点
        append my-draw reduce ['line arrow-vertex up-arrow-start]
        append my-draw reduce ['line arrow-vertex down-arrow-start]
    ][
        line-start: source-x + arrow-offset 
        line-end: dest-x - arrow-offset
        up-arrow-start: as-pair (line-end - arrow-offset) y + 3
        down-arrow-start: as-pair (line-end - arrow-offset) y - 3
        arrow-vertex: as-pair line-end y ; 箭头的顶点
        append my-draw reduce ['line up-arrow-start arrow-vertex]
        append my-draw reduce ['line down-arrow-start arrow-vertex]
    ]
    ; 画备注 计算中间点，计算备注长度
    note-length: font-width * length? note
    middle-x: (line-end - line-start) / 2 + line-start
    note-y: y - font-height
    append my-draw reduce ['font small-font 'text (as-pair middle-x - (note-length / 2) note-y) note]
    ;print ["开始绘制虚线" source dest (as-pair line-start y) as-pair line-end y]
    while [line-start < line-end] [
        if line-start > line-end [ break ]
        temp-end: line-start + part-length
        if temp-end > line-end [temp-end: line-end]
        append my-draw compose [line (as-pair line-start y) (as-pair temp-end y)]
        line-start: line-start + part-length + gap
    ]
]

draw-node: function ["画节点" nodes] [
    x: edge
    y: edge
    foreach node nodes [
        print ["开始画节点" node as-pair x y]
        append my-draw compose [font default-font]
        append my-draw reduce ['text (as-pair x + 5 y + 5) node]
        append my-draw compose [box (as-pair x y) (as-pair x + node-width y + node-height)]
        ; 画竖线
        append my-draw compose [line-width 1 pen black]
        line-start-x: x + (node-width / 2)
        put nodes-x node line-start-x ; 记下此节点的x轴坐标
        append my-draw compose [line (as-pair line-start-x y + node-height) (as-pair line-start-x vline-height)]
        x: x + node-width + node-gap
    ]
    return y + node-height
]

draw-processor: function ["画处理条" node "节点名称" y "y轴坐标" height "高度"] [
    x: select nodes-x node
    append my-draw compose [line-width 1 pen black fill-pen 128.100.50 box (as-pair x - 5 y) (as-pair x + 5 y + height)]
]

draw-flow: function ["画流程" y] [
    tmp-processor: #() ; 临时记录处理块的y轴坐标，key是节点，value是y轴坐标列表
    processor: [] ; 最终的处理块y轴坐标，每个处理块一个坐标
    path: [] ; 记录调用流程，用于区分大的处理块是否结束
    foreach flow data [
        y: y + flow-gap
        draw-arrow flow/1 flow/2 y flow/3
        source: select tmp-processor flow/1
        dest: select tmp-processor flow/2
        either source = none 
            [put tmp-processor flow/1 reduce [y]]
            [append source y]
        either dest = none 
            [put tmp-processor flow/2 reduce [y]]
            [append dest y]

        either (last path) == append copy flow/2 flow/1 [
            take/last path ; 与上一个flow是一对，删除最后一个
            if empty? path [ ; 删除最后一个flow之后，如果path为空，说明大的处理块结束了
                print ["一个大的处理块结束了" select tmp-processor flow/2]
                append processor copy tmp-processor ; 把整个流程全部转移到最终的记录中，清空临时记录，从头开始
                clear tmp-processor
            ]
        ] [
            append path reduce [append copy flow/1 flow/2]
        ]
    ]
    ; 画处理块
    foreach process processor [
        foreach [node list] body-of process [
            node-y: (first list) - 3 ; 处理块比流程箭头稍高一些
            height: (last list) - node-y + 3
            draw-processor node node-y height
        ]
    ]
]

my-draw: []
nodes: #()
foreach flow data [put nodes flow/1 1] ; 提取所有节点
node-count: length? keys-of nodes
width: (node-count * node-width) + ((node-count - 1) * node-gap) + (edge * 2)
flow-count: length? data
height: flow-count * flow-gap + (edge * 2) + node-height
draw-size: as-pair width height
vline-height: height

y: draw-node keys-of nodes ; 画节点
draw-flow y ; 画流程箭头

view [
    title "流程图"
    backdrop white
    origin 0x0
	base 250.250.250 draw-size 
	draw my-draw 
]
