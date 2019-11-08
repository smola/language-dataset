//免费刷新：直接刷新			1000
//当天已经刷新过				3001
//发布超过30天：发新帖		3030
//7天内超过4次:重新编辑		3070
//免费刷新用完：不管 			3100
//个人删除 					3404
//需返回修改 				3401
//订单异常					3500
//审核不通过 				3501
//刷新成功 					3502
//刷新失败 					3503
//订单不存在 				3504
//编辑帖子成功 				3505
//编辑帖子失败 				3506
//编辑帖子异常 				3507
//--------------------------------------
//账户正常					2000
//密码错 					4000
//未实名认证 				4001
//认证超时 					4002
//用户名不存在				4003
//账户异常					4004
//账户需要设置cookie			4005
//--------------------------------------
Dim id, chromeSrc, mysql,houseInfo
configStatus = config()
If configStatus = 0 Then
    call checkChrome()
    Call main()
End If
//--------------------------------------
Function config()
    status=0
    Path = Plugin.Sys.GetDir(0)
    Call Plugin.File.WriteINI("Command", "name", "58",Path+"\58config.ini")
    chromeSrc = Plugin.File.ReadINI("Command", "chromeSrc", Path + "\58config.ini")
    If (chromeSrc = "") Then
        status=-1
        MessageBox "请设置chrome目录"
    End If
    config=status
End Function
Function main()
    Call mySqlConnect()
    Rem fLogin
    Call start()
    //MessageBox "login"
    status = login()
    If status = 0 Then 
        status= houseList()
    End If
    call updateStatus(user,status)
    Goto fLogin
End Function
Function loadJquery()
    execjs ("window.$=null;")
    execjs("var script=document.createElement('script');script.type='text/javascript';script.src='https://cdn.bootcss.com/jquery/2.1.2/jquery.min.js';document.getElementsByTagName('head')[0].appendChild(script); ")
    Do
        Delay 10
        flag = execjs("$===null")
        flag=CBool(flag)
    Loop While flag
End Function
Function refresh(houseId)
    url = "https://refreshplus.vip.58.com/refreshplus/pc/normalrefreshpage?infoId=" + cstr(houseId) + "&source=1#/immediate"
    Call Plugin.chrome.load(id, url)
    // 等待载入完成
    Do
        Delay 10
    Loop While Plugin.chrome.is_loading(id)
    Call find("document.querySelectorAll('div').length","查找div")
    Call loadJquery() 
    //        length = find("document.querySelectorAll('.nav-item').length", "查找刷新页面的tabs")
    length = find("$('a[href$=immediate]').length", "查找立即刷新a标签")
    rHouseStatus=-1
    If length > 0 Then
        execjs ("$('a[href$=immediate]')[0].click()")
        Delay 300
        //剩余刷新次数
        //rCount=execjs("$('p.warning-message.important').text().trim().match(/\d{1,}/)[0]")
        //        MessageBox "剩余" & rCount & "次"
        //Call find("$('form.form-container.immediate').length;","查找immediate")
        Delay 1000
        Call execjs("var wContent=$('form.form-container.immediate').text().trim().replace(/[\n\s]/g,'')||$('form.yyset').text().trim().replace(/[\s\n]/g,'');")
        call execjs("function getStatus() {"&_
    					"if (wContent.indexOf('近7天免费刷新已到上限') > -1) {"&_
        					"return 3070"&_
    					"} else if(wContent.indexOf('30天不享有免费刷新') > -1||wContent.indexOf('免费刷新次数已用完，您可尝试付费刷新') > -1) {"&_
        					"return 3030"&_
    					"} else if (wContent.indexOf('本次刷新免费') > -1||/账号剩余刷新资源[^0]/.test(wContent)) {"&_
        					"return 1000"&_
    					"} else {"&_
        					"return - 1"&_
    					"}"&_
					"}")
        rHouseStatus=execjs("getStatus();")
        If rHouseStatus = "" Then 
            rHouseStatus=-1
        End If
        rHouseStatus=cint(rHouseStatus)
        If rHouseStatus = 1000 Then 
            Call find("$('button:contains(确定刷新)').length", "查找确认刷新按钮")
            Call execjs("$('button:contains(确定刷新)').click();")
            Delay 1000
            rResult = find("$('.result__title-text:contains(立即刷新成功)').length||($('.success-title').text()=='立即刷新成功'?1:0)", "查找刷新成功结果")
            If rResult = "" Then 
                rResult=0
            End If
            rResult = cint(rResult)
            //刷新成功 					3502
            //刷新失败 					3503
            If rResult > 0 Then 
                //刷新成功
                rHouseStatus=3502
            Else 
                //刷新失败
                rHouseStatus=3503
            End If
        ElseIf rHouseStatus = 3070 Then
            //重新编辑
            rHouseStatus=editHouseInfo(houseId)
        End If
    End If
    refresh=rHouseStatus
End Function
Function editHouseInfo(houseId)
    Rem rEditHouseInfo
    editStatus=3507
    //获取详情
    url = "https://sy.58.com/shangpu/" + houseId + "x.shtml"
    Call Plugin.chrome.load(id, url)
    // 等待载入完成
    Do
        Delay 10
    Loop While Plugin.chrome.is_loading(id)
    length = find("$('.house-title h1').length", "查找登录窗口")
    Call execjs("var editData={};"&_
    			"editData.surplusMonth = $('span:contains(剩余租期:)').length?$('span:contains(剩余租期:)').next().text().trim().match(/(\d+)(.*)/)[1]:'';"&_
				"editData.surplusMonthUnit = $('span:contains(剩余租期:)').length?$('span:contains(剩余租期:)').next().text().trim().match(/(\d+)(.*)/)[2]:'';"&_
				"editData.zuJInUnit=$('span.house_basic_title_money_num').length?$('span.house_basic_title_money_num').next().text():'';"&_
				"editData.zuJIn=$('span.house_basic_title_money_num').text();"&_
				"editData.status=$($('.house_basic_title_content').html().replace(/[\n\t]/g,'').replace(/\&nbsp\;/g,'')).find('span:contains(状态)').next().text().trim();"&_
				"editData.title=$('.house-title h1').text().trim().substr(0,30);"&_
				"editData.zrf=$('span.house_basic_title_money_zrf').length?$('span.house_basic_title_money_zrf').text().match(/(\d+)(.*)/)[1]:'';"&_
				"editData.zrfUnit=$('span.house_basic_title_money_zrf').length?$('span.house_basic_title_money_zrf').text().match(/(\d+)(.*)/)[2]:'';")
    editData = execjs("JSON.stringify(editData)")
    TracePrint editData
    length = find("editData&&editData.title?editData.title.length:0","获取title长度")
    If length = "" Then 
        length=0
    End If
    If cint(length) = 0 Then 
        Goto rEditHouseInfo
    End If
    Rem fEditDetailPage
    //回到列表页面点击编辑进入详情编辑页面
    url = "https://fangmypost.58.com/fang/infoall/notabs"
    Call Plugin.chrome.load(id, url)
    // 等待载入完成
    Do
        Delay 10
    Loop While Plugin.chrome.is_loading(id)
    Call find("$('body .rightside-publish-content .my-item-item').length", "个人中心房产列表")
    Delay 1000
    Call execjs("$('#update" + houseId + "').click();")
    url = "http://post.58.com/fang/update/" + cstr(houseId) + "?PGTID=${window.getGTID()}&ClickID=2"
    TracePrint url
    Call Plugin.chrome.exec_js(id,"$(function(){window.location.href=`"+url+"`})")
    Delay 3000
    length = find("$('#postForm div').length", "获取form下面的div个数，以证明页面渲染完成")
    If length > 0 Then 
        TracePrint "var editData="&editData&";"
        Call execjs("var editData=" & editData & ";")
        Call execjs("$('[name=title] input').val(editData['title']);"&_
					"$('[name=params_205] label:contains(' + editData['status'] + ')').parent().click();"&_
					"$('[name=jiaGe05] input').val(editData['zuJIn']);"&_
					"$('[name=jiageDanwei] .optiondef').show();"&_
					"$('[name=jiageDanwei] .optiondef li:contains(' + editData['zuJInUnit'] + ')').click();"&_
					"$('[name=params_215] input').val(editData['surplusMonth']);"&_
					"$('[name=params_214] input').val(editData['zrf']);")
        execjs("window.scrollTo(0,99999);")
        Delay 1000
        execjs ("window.scrollTo(0,0);")
        Delay 1000
        execjs ("window.scrollTo(0,99999);")
        Delay 1000
        execjs ("$('.submit_wrap span').trigger('click');")
        Delay 5000
        index = find("document.title.indexOf('息发布成功')","查找发布成功标题")
        If index = "" Then 
            index=-1
        End If
        index = cint(index)
        If index > 0 Then 
            //发布成功
            MessageBox "发布成功"
            editStatus=3505
        Else 
            TracePrint "重新编辑提交异常"
            Goto rEditHouseInfo
        End If
    Else 
        Goto fEditDetailPage
    End If
    editHouseInfo=editStatus
End Function
Function houseList()
    Rem fHouseList
    //订单列表
    houseStatus=3502
    url = "https://fangmypost.58.com/fang/infoall/notabs"
    Call Plugin.chrome.load(id, url)
    // 等待载入完成
    Do
        Delay 10
    Loop While Plugin.chrome.is_loading(id)
    length = find("$('body .rightside-publish-content .my-item-item').length","个人中心房产列表")
    If length > 0 Then
        houseId = houseInfo(0)
        TracePrint "houseId:"+houseId
        length = find("$('.my-item-item-header-code:contains(" + houseId + ")').length", "查找是否存在指定房屋id的信息")
        If length > 0 Then 
            Call execjs("var houseItem=$('.my-item-item-header-code:contains(" + houseId + ")').parents('.my-item-item-tb:eq(0)')")
            execjs("var rDate=houseItem.find('p.my-item-item-content-left-date').length?new Date(houseItem.find('p.my-item-item-content-left-date').text().trim()).getTime():0")
            state = execjs("houseItem.attr('state')")
            //===============================================
            //state 
            //显示中:1
            //未实名认证:2
            //===============================================
            If state = "" Then
                state = - 1
            Else
                state=cint(state)
            End If
            If state = 2 Then
                //账户需未实名认证:4001
                houseStatus = 4001
            ElseIf state = - 1  Then
                houseStatus=3502
                MessageBox "未处理异常4564"
            Else 
                Call execjs("var statusContent=houseItem.find('.my-item-item-tb-content-states').text().trim().replace(/\n/g,'').replace(/\s/g,'').substr(0,20)")
                call execjs("function getHouseStatusJs(){"&_
                "if(statusContent.indexOf('显示中')>-1){"&_
                "return 1"&_
                "}else if(statusContent.indexOf('个人删除')>-1){"&_
                "return 3404"&_
                "}else if(statusContent.indexOf('审核不通过')>-1){"&_
                "return 3501"&_
                "}else if(statusContent.indexOf('返回修改')>-1){"&_
                "return 3401"&_
                "}"&_
                "}")
                houseStatus=execjs("getHouseStatusJs();")
            End If
            If houseStatus = 1 Then 
                Call execjs("function isToday(str) {"&_
    						"if (new Date(str).toDateString() === new Date().toDateString()) {"&_
        						"return true;"&_
    						"} else if (new Date(str) < new Date()){"&_
        						"return false;"&_
    						"}"&_
						"}")
                isToday = execjs("isToday(rDate);")
                If isToday = "" Then 
                    isToday=False
                End If
                isToday = CBool(isToday) 
                If isToday = - 1  Then  
                    houseStatus = 3001
                Else 
                    houseStatus = refresh(houseId)
                End If
            End If
        Else 
            houseStatus=3504
            TracePrint "未查找到此条记录"
        End If
    Else 
        houseStatus=-1
        TracePrint "个人中心房产列表"
    End If
    houseList=houseStatus
End Function
Function find(str,c)
    TracePrint "find   "+c
    TracePrint str
    count = 0
    length=0
    Do
        Delay 150
        length = execjs(str)
        If length = "" Then
            length=0
        End If
        length = CInt(length)
        count = count + 1
        TracePrint count
    Loop While length = 0 and count < 200
    find=length
End Function
Function findp2(str,c,pageId)
    TracePrint "find   "+c
    TracePrint str
    count = 0
    length=0
    Do
        Delay 150
        length = execjsp2(str,pageId)
        If length = "" Then
            length=0
        End If
        length = CInt(length)
        count = count + 1
        TracePrint count
    Loop While length = 0 and count < 200
    findp2=length
End Function
Function execjsp2(str,pageid)
    TracePrint "execjs: "& str
    result= Plugin.chrome.exec_js(pageid,str)
    execjsp2=result
End Function
Function execjs(str)
    TracePrint "execjs: "& str
    result= Plugin.chrome.exec_js(id,str)
    execjs=result
End Function
Function login()
    Call clearAllCookie() 
    user = getUser()
    cookie = user(5)
    If cookie <> "" Then 
        Call setCookie(cookie)
    End If
    call Plugin.chrome.activate(id)
    Call Plugin.chrome.load(id, "https://passport.58.com/logout")
    // 等待载入完成
    Do
        Delay 10
    Loop While Plugin.chrome.is_loading(id)
    length = find("$('.passwordlogin').length", "查找登录窗口")
    If length > 0 Then
        loginStatus = 0
        //判断登录窗口类型
        Call Plugin.chrome.exec_js(id,"if($('.change_qrcode[type_attr=pclogin]').length){$('.pop_head .qrcode').click()}")
        Delay 500
        Call Plugin.chrome.activate(id)
        //        Delay 500
        //        MoveTo 748, 379
        //        LeftClick 2
        //        SayString user(0)
        Call Plugin.chrome.exec_js(id, "$('#username').val('').focus().val('"+user(0)+"');")
        //        Delay 500
        //        MoveTo 749, 441
        //        LeftClick 2
        //        SayString user(1)
        Call Plugin.chrome.exec_js(id, "$('#password').val('').focus().val('"+user(1)+"');")
        Delay 500
        Call Plugin.chrome.exec_js(id,"verifyobject.isverifyaccount=true;verifyobject.isverifyvalidcode1=true;verifyobject.isverifypwd=true;")
        // 点击登录按钮
//        MoveTo 676, 524
//        Delay 500
//        LeftClick 1
        Call Plugin.chrome.exec_js(id, "$('#btn_account').click();")
        
        count = 0 
        Do
            loginStatus = 0
            Delay 500
            result = execjs("$('.password_msg.msg:visible').length||0")
            If result = "" Then
                result=0
            End If
            If CInt(result) = 1 Then  
                loginStatus = 4000
                TracePrint Trim(execjs("$('.password_msg.msg:visible').parent().text()"))
            End If
            result1 = execjs("$('.write.write58').length||0")
            If result1 = "" Then
                result1=0
            End If
            If CInt(result1) = 1 Then  
                loginStatus =4004
                TracePrint "您的账户存在安全风险"
            End If
            url = Plugin.chrome.get_url(id)
            TracePrint url
            count=count+1
        Loop While InStr(url, "my.58.com/index") = 0 and count < 10
    Else
        loginStatus=4004
        MessageBox "未处理异常"
    End If
    login=loginStatus
End Function
Function updateStatus(userInfo, status)
    TracePrint  "updateStatus:"&status
    //每次只能更新一个字段
    //获取待插入的id
    sql = "select max(id) as id from `log`"
    TracePrint sql
    call Plugin.Super_MySQL.mysql_query(mysql, sql)
    store = Plugin.Super_MySQL.store_result(mysql)
    rows = Plugin.Super_MySQL.num_rows(store)
    If rows > 0 Then 
        maxId = Plugin.Super_MySQL.fetch_text(store, "id")
        If maxId = "" Then 
            maxId="1"
        End If
        maxId = cstr(cint(maxId)+1)
        //insert id 
        sql = "INSERT into log (id) value(" + maxId + ")"
        TracePrint sql
        call Plugin.Super_MySQL.mysql_query(mysql, sql)
        store = Plugin.Super_MySQL.store_result(mysql)
        call Plugin.Super_MySQL.num_rows(store)
        //update house_key
        sql = "UPDATE log SET `house_key`='" + userInfo(3) + "' WHERE id=" + maxId
        TracePrint sql
        call Plugin.Super_MySQL.mysql_query(mysql, sql)
        store = Plugin.Super_MySQL.store_result(mysql)
        call Plugin.Super_MySQL.num_rows(store)
        //update status
        sql = "UPDATE log SET `status`=" + cstr(status) + " WHERE id=" + maxId
        TracePrint sql
        call Plugin.Super_MySQL.mysql_query(mysql, sql)
        store = Plugin.Super_MySQL.store_result(mysql)
        call Plugin.Super_MySQL.num_rows(store)
        //update create_date
        sql = "UPDATE log SET `create_date`=now() WHERE id=" + maxId
        TracePrint sql
        call Plugin.Super_MySQL.mysql_query(mysql, sql)
        store = Plugin.Super_MySQL.store_result(mysql)
        call Plugin.Super_MySQL.num_rows(store)
    Else 
        TracePrint "更新数据失败"
    End if
End Function
Function start()
    Call Plugin.chrome.quit()
    Delay 1000
    // 启动浏览器
    If Plugin.chrome.launch(chromeSrc, "--user-data-dir") = False Then
        TracePrint "启动失败"
        Call start()
    Else 
        // 获取启动浏览器后的默认标签
        id = Plugin.chrome.get_ids()
        If id = "" Then
            call start()
        End If
        page2 = Plugin.chrome.create()
        url = "chrome-extension://fngmhnnpilhplaeedifhccceomclgfbg/popup.html?url=http://passport.58.com&id=-1&incognito=false"
        Call Plugin.chrome.load(page2, url)
        call Plugin.chrome.activate(id)
    End if
End Function
Function object()
    Dim o
    Set o = CreateObject("Scripting.Dictionary")
    o.Add "hash", "1"
    MessageBox o.Item("hash")
    object=1
End Function
Function publish()
    Call find("$('#commonTopbar_login').length", "查找登录用户")
    Delay 1000
    //发布信息
    //Call Plugin.chrome.exec_js(id, "clickLog('from=my_post_btn');")
    Call Plugin.chrome.exec_js(id,"$(function(){window.location.href=`https://post.58.com/fang/1/14/s5?PGTID=${window.getGTID()}&ClickID=2`})")
    // 等待载入完成
    Delay 1000
    call find("$('.submit_wrap').length","查找发布按钮")
    //分类
    Call Plugin.chrome.exec_js(id,"$('div[name=fenlei] [data-value=511570]').click()")
    //供求
    Call Plugin.chrome.exec_js(id,"$('div[name=type] [data-value=0]').click()")
    //商铺类型
    Call Plugin.chrome.exec_js(id,"$('div[name=params_204] .optiondef').show()")
    Call Plugin.chrome.exec_js(id,"$('div[name=params_204] .optiondef li[val=2]').click()")
    //商铺性质
    Call Plugin.chrome.exec_js(id,"$('div[name=params_122] .optiondef').show()")
    Call Plugin.chrome.exec_js(id,"$('div[name=params_122] .optiondef li[val=2]').click()")
    //当前状态
    Call Plugin.chrome.exec_js(id,"$('div[name=params_205] [data-value=1]').click()")
    //经营行业
    Call Plugin.chrome.exec_js(id,"$('div[name=params_22701] .optiondef').show()")
    Call Plugin.chrome.exec_js(id,"$('div[name=params_22701] .optiondef li[val=511574]').click()")
    Call Plugin.chrome.exec_js(id,"$('div[name=params_20601] .optiondef').show()")
    Call Plugin.chrome.exec_js(id,"$('div[name=params_20601] .optiondef li[val=511591]').click()")
    //建筑面积
    Call Plugin.chrome.exec_js(id,"$('div[name=mianJi] input').val(99)")
    //楼层
    Call Plugin.chrome.exec_js(id,"$('div[name=params_109] .optiondef').show()")
    //单层
    Call Plugin.chrome.exec_js(id,"$('div[name=params_109] .optiondef li[val=1]').click()")
    Call Plugin.chrome.exec_js(id,"$('div[name=suoZaiLouCeng] #suoZaiLouCeng').val(4)")
    Call Plugin.chrome.exec_js(id,"$('div[name=zongLouCeng] #zongLouCeng').val(6)")
    //面积
    Call Plugin.chrome.exec_js(id,"$('div[name=params_207] input').val(99)")
    //层高
    Call Plugin.chrome.exec_js(id,"$('div[name=params_208] input').val(66)")
    //进深
    Call Plugin.chrome.exec_js(id,"$('div[name=params_209] input').val(11)")
    //是否临街
    Call Plugin.chrome.exec_js(id,"$('div[name=params_211] div[data-value=1]').click()")
    //商铺配套
    Call Plugin.chrome.exec_js(id,"$('div[name=params_110] div[data-value=1]').click()")
    //商铺位置
    //省
    Call Plugin.chrome.exec_js(id,"$('div[name=localArea01] .optiondef').show()")
    Call Plugin.chrome.exec_js(id,"$('div[name=localArea01] .optiondef li[val=1142]').click()")
    //市
    Call Plugin.chrome.exec_js(id,"$('div[name=localDiduan01] .optiondef').show()")
    Call Plugin.chrome.exec_js(id,"$('div[name=localDiduan01] .optiondef li[val=1195]').click()")
    //区
    Call Plugin.chrome.exec_js(id,"$('div[name=xiangXiDiZhi] #xiangXiDiZhi').val('幽幽')")
    //客流人群
    Call Plugin.chrome.exec_js(id,"$('div[name=params_210] div[data-value=1]').click()")
    //售价
    Call Plugin.chrome.exec_js(id,"$('div[name=jiaGe06] #jiaGe06').val('20000')")
    //租金
    Call Plugin.chrome.exec_js(id,"$('div[name=jiaGe05] #jiaGe05').val('20000')")
    Call Plugin.chrome.exec_js(id,"$('div[name=jiageDanwei] .optiondef').show()")
    Call Plugin.chrome.exec_js(id,"$('div[name=jiageDanwei] .optiondef li[val=1]').click()")
    //支付方式
    Call Plugin.chrome.exec_js(id,"$('div[name=params_119] #params_119').val('3')")
    Call Plugin.chrome.exec_js(id,"$('div[name=params_118] #params_118').val('1')")
    //起租期
    Call Plugin.chrome.exec_js(id,"$('div[name=params_116] #params_116').val('1')")
    //免租期
    Call Plugin.chrome.exec_js(id,"$('div[name=params_117] #params_117').val('1')")
    //标题
    Call Plugin.chrome.exec_js(id,"$('div[name=title] #title').val('出租朝阳国贸商业街店铺1')")
    //描述
    Call Plugin.chrome.exec_js(id,"$('#edui1 iframe').contents().find('body').html('<p>（65m2-156m2）多种户型、多种房子、有无家具、房屋买卖、详情咨询、可进店铺\n')")
    //联系人
    Call Plugin.chrome.exec_js(id,"$('div[name=contactName] #contactName').val('王先生')")
    //上传图片
    Call Plugin.chrome.set_file_input_files(id, ".imgUpload input", "d:/temp/1.jpg||d:/temp/2.jpg||d:/temp/3.jpg")
End Function
Function quit()
    MsgBox "脚本执行完毕"
    // 退出浏览器（如果不退出浏览器下次执行"launch"方法用同一个"--user-data-dir"启动参数会启动失败）
    Call Plugin.chrome.quit()
End Function
Function checkChrome()
    HwndEx = Plugin.Window.Search("Google")
    dim MyArray
    MyArray = Split(HwndEx, "|")
    If UBound(MyArray) >= 0 Then
        //        result = MsgBox("是否关闭chrome浏览器？", 1)
        result=1
        If result = 1 Then
            i=0
            For UBound(MyArray)
                Call Plugin.Window.CloseEx(Clng(MyArray(i)))
                i=i+1
            Next
            Delay 300
        Else
            EndScript
        End If
    End If
End Function
Function mySqlConnect()
    mysql  = Plugin.Super_MySQL.mysql_connect("101.201.49.69", "refresh", "Dianzhijia@1", "datarefresh", 3306)
End Function
Function getUser()
    Rem fGetUser
    //获取一条没有被锁定，没有被忽略的房屋信息
    sql = "SELECT * FROM house_list WHERE `block` IS NULL AND username IS NOT NULL ORDER BY RAND( ) LIMIT 1"
    //sql = "SELECT * FROM house_list WHERE `username`='13272718539' ORDER BY RAND( ) LIMIT 1"
    TracePrint sql
    call Plugin.Super_MySQL.mysql_query(mysql, sql)
    store = Plugin.Super_MySQL.store_result(mysql)
    rows = Plugin.Super_MySQL.num_rows(store)
    If rows > 0 Then
        //读字段
        houseListId = Plugin.Super_MySQL.fetch_text(store, "id")
        userName = Plugin.Super_MySQL.fetch_text(store, "username")
        TracePrint "userName:" + userName
        password = Plugin.Super_MySQL.fetch_text(store, "password")
        TracePrint "parrword:" + parrword
        key = Plugin.Super_MySQL.fetch_text(store, "key")
        url = Plugin.Super_MySQL.fetch_text(store, "url")
        If password = "" Then 
            MessageBox "password"+password
        End If
        //锁定这条数据
        sql = "UPDATE house_list SET `block`=1 WHERE id=" + houseListId
        TracePrint sql
        Call Plugin.Super_MySQL.mysql_query(mysql, sql)
        //MessageBox userName+"&"+password+"&"+houseListId+"&"+key+"&"+url
        cookie = getCookie(userName)
        TracePrint "cookie:" + cookie
        houseId = getHouseId(url)
        houseInfo=Array(houseId)
        getUser = Array(userName, password,houseListId,key,url,cookie)
    Else 
        //getUser=Array("-1")
        TracePrint "获取house_list信息异常"
        Goto fGetUser
    End If
End Function
Function getCookie(userName)
    Text = Plugin.File.ReadFileEx("d:\cookie\"+userName+".txt")
    dim MyArray
    MyArray = Split(Text, "|")
    cookieValue=""
    If UBound(MyArray)>=0 Then
        i=0
        For UBound(MyArray)
            //下面这句是得到文本内容
            cookieValue=cookieValue+ Cstr(MyArray(i))
            i=i+1
        Next
    End If
    TracePrint cookieValue
    getCookie = cookieValue 
End Function
Function setCookie(value)
    Call Plugin.chrome.activate(page2)
    TracePrint "setCookie"
    //Call execjsp2("function setCookie(name, value, domain, expirationDate) {    var Days = 30;    var exp = new Date();    exp.setTime(exp.getTime() + Days * 24 * 60 * 60 * 1000);    let cookie = name + '=' + value + ';expires=' + new Date(expirationDate * 1000).toGMTString() + '; path=/;';   if (domain) {        cookie += 'domain=' + domain + ';';    }    console.log(cookie);document.cookie = cookie;};",page2)
    //Call execjsp2("var cookieArr=JSON.parse(decodeURIComponent('" + value + "'))",page2) 
    //Call execjsp2("cookieArr.map(item => {    let {        domain,        name,        value,        expirationDate    } = item;    if (expirationDate) {        setCookie(name, value, domain, expirationDate);    }})",page2)
    //    Call execjsp2("var cookieList=cookieArr.map((item, index) => {"&_
//    				"return {"&_
//        			"	'domain': item.domain,"&_
//        			"	'expirationDate': item.expirationDate,"&_
//        			"	'hostOnly': false,"&_
//        			"	'httpOnly': false,"&_
//        			"	'name': item.name,"&_
//        			"	'path': '/',"&_
//        			"	'sameSite': 'no_restriction',"&_
//        			"	'secure': false,"&_
//        			"	'session': false,"&_
//        			"	'storeId': '0',"&_
//        			"	'value': item.value,"&_
//        			"	'id': (index + 1)"&_
//    				"}"&_
//				"})",page2)
    Call execjsp2("$('#pasteButton').click()", page2)
    //Call execjsp2("var cookieValue=`"+cstr(value)+"`", page2)
    Call findp2("$('#pasteCookie textarea:visible').length", "find textarea", page2)
    Call Plugin.Sys.SetCLB(value)
    MoveTo 606, 294
    Delay 300
    LeftClick 2
    Delay 300
    KeyDown 17, 1
    KeyPress 86, 1
    KeyUp 17, 1
    Delay 300
    //    Call execjsp2("$('#pasteCookie textarea:visible').text(JSON.stringify(cookieValue))", page2)
    //    Delay 1000
    Call execjsp2("$('#submitButton').click()", page2)
    Delay 2000
end Function
Function clearAllCookie()
    Call Plugin.chrome.activate(page2)
    url = "chrome-extension://fngmhnnpilhplaeedifhccceomclgfbg/popup.html?url=http://passport.58.com&id=-1&incognito=false"
    Call Plugin.chrome.load(page2, url)
    // 等待载入完成
    Do
        Delay 10
    Loop While Plugin.chrome.is_loading(id)
    Call findp2("$('#cookieSearchCondition:visible').length", "查找页面元素",page2)
    Delay 1000 
    Call execjsp2("$('#deleteAllButton')[0].click()",page2)
    Call findp2("$('#noCookies:contains(没有cookie记录)').length", "没有cookie记录！",page2)
End Function
Function getHouseId(url)
    regstr = url
    regin1 = Instr(regstr, "shangpu/")
    regin2 = Instr(regstr, "x.shtm")
    regl1 = Len("shangpu/")
    regr1 = Mid(regstr, regin1 + regl1, regin2 - regin1 - regl1)
    getHouseId=regr1
End Function