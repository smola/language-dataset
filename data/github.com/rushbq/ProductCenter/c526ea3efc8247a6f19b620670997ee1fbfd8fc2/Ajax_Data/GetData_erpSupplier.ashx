<%@ WebHandler Language="C#" Class="GetData_Supplier" %>

using System;
using System.Web;
using System.Linq;
using System.Text;
using System.Data;
using System.Data.SqlClient;
using System.Collections.Generic;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using PKLib_Data.Controllers;
using PKLib_Data.Assets;

public class GetData_Supplier : IHttpHandler
{

    /// <summary>
    /// 取得ERP 供應商資料(Ajax)
    /// 目前使用:外驗查檢表ProdCheck
    /// </summary>
    public void ProcessRequest(HttpContext context)
    {
        //[接收參數] 查詢字串
        string searchVal = context.Request["keyword"];
        string corp = context.Request["corp"];


        //----- 宣告:資料參數 -----
        SupplierRepository _data = new SupplierRepository();
        Dictionary<int, string> search = new Dictionary<int, string>();


        //----- 原始資料:條件篩選 -----
        if (!string.IsNullOrWhiteSpace(searchVal))
        {
            search.Add((int)Common.mySearch.Keyword, searchVal);
        }

        if (!string.IsNullOrWhiteSpace(corp))
        {
            search.Add((int)Common.mySearch.Corp, corp);
        }

        //----- 原始資料:取得所有資料 -----
        var query = _data.GetERPDataList(search);


        //----- 資料整理:顯示筆數 -----
        var data = query
            .Select(fld =>
             new
             {
                 ID = fld.ERP_SupID,
                 Label = "(" + fld.Corp_Name + ") " + fld.ERP_SupName + "-" + fld.ERP_SupID,
                 CorpID = fld.Corp_UID
             })
            .Take(100);


        //----- 資料整理:序列化 ----- 
        string jdata = JsonConvert.SerializeObject(data, Newtonsoft.Json.Formatting.Indented);

        /*
         * [回傳格式] - Json
         * data：資料
         */

        //輸出Json
        context.Response.ContentType = "application/json";
        context.Response.Write(jdata);


    }

    public bool IsReusable
    {
        get
        {
            return false;
        }
    }

}