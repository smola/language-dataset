--�����޲δ洢����
create proc usp_ShopMenuList
as
begin
	select * from ShopMenu where MDataStatus<>99
end
go
exec usp_ShopMenuList
go

-----------------------------------------------------------------

--�����вδ洢����
create proc usp_ShopMenuListByCityName
@cityName nvarchar(30)
as
begin
	select CPName,CName,SName,MType,MName,Mprice from ShopMenu 
	inner join ShopModel on ShopMenu.MShopId=ShopModel.SId
	inner join View_CityData on ShopMenu.MCityId=CId
	where CName=@cityName
end
go
exec usp_ShopMenuListByCityName @cityName='������'
go

-----------------------------------------------------------------

--�����з���ֵ�Ĵ洢���̣�����ֵֻ����int���ͣ�--������
create proc usp_UpdateShopModelStatus
as
begin
	update ShopModelBak set SDataStatus=0
	return (select count(1) from ShopMenuBak) --return @@rowcount
end
go
declare @total int
exec @total=usp_UpdateShopModelStatus
select @total
go

-----------------------------------------------------------------

--��������������Ĵ洢���̣��Ƚϳ��ã�
create proc usp_GetShopMenus
@cityName nvarchar(30),@total int output
as
begin
	select CPName,CName,SName,MType,MName,Mprice from ShopMenu 
	inner join ShopModel on ShopMenu.MShopId=ShopModel.SId
	inner join View_CityData on ShopMenu.MCityId=CId
	where CName=@cityName
	select @total=count(1) from ShopMenu
end
go
declare @toal int
exec usp_GetShopMenus '������',@toal output
select @toal
go

-----------------------------------------------------------------

--�ۺ�Ӧ��-���ֲ������
if exists(select * from sysobjects where name='usp_AllPmsTest')
	drop proc usp_AllPmsTest
go
create proc usp_AllPmsTest
@cityName nvarchar(30),
@id int output
as
begin
	insert into ShopModelBak values(@cityName,1,1)
	set @id=@@identity

	select CPName,CName,SName,MType,MName,Mprice from ShopMenu 
	inner join ShopModel on ShopMenu.MShopId=ShopModel.SId
	inner join View_CityData on ShopMenu.MCityId=CId
	where CName=@cityName

	return (select count(1) from ShopMenu)
end
go
declare @total int,@id int
exec @total=usp_AllPmsTest '������',@id output
select @id Id,@total total
go

--�ۺ�Ӧ��--��ҳ
if exists(select * from sysobjects where name='usp_GetShopMenus_Page')
	drop proc usp_GetShopMenus_Page
go
create proc usp_GetShopMenus_Page
@mIndex int,@mCount int=7
as
begin
	select * from(select row_number() over(order by MType) Id, CPName,CName,SName,MType,MName,Mprice from ShopMenu 
	inner join ShopModel on ShopMenu.MShopId=ShopModel.SId
	inner join View_CityData on ShopMenu.MCityId=CId) as temp
	where Id between (@mIndex-1)*@mCount  and (@mIndex)*@mCount
	return (select count(1) from ShopMenu)
end
go
declare @total int,@index int=1,@count int=9
exec @total=usp_GetShopMenus_Page @index,@count
select @index Mindex,@count MCount, @total MTotal
-----------------------------------------------------------------
--ADO.Net����
-----------------------------------------------------------------------
----�����вδ洢����
----using (SqlConnection conn = new SqlConnection(connStr))
----{
----    conn.Open();
----    string sql = "usp_ShopMenuListByCityName";
----    var pms = new SqlParameter("@cityName", "������");
----    using (SqlCommand cmd = new SqlCommand(sql, conn))
----    {
----        cmd.Parameters.Add(pms);
----        cmd.CommandType = CommandType.StoredProcedure;
----        var reader = cmd.ExecuteReader();
----    }
----}

-----------------------------------------------------------------------
----ִ���з���ֵ�Ĵ洢����
----using (SqlConnection conn = new SqlConnection(connStr))
----{
----    using (SqlCommand cmd = new SqlCommand("usp_UpdateShopModelStatus", conn))
----    {
----        var pms = new SqlParameter("@Count", SqlDbType.Int);
----        pms.Direction = ParameterDirection.ReturnValue;
----        cmd.Parameters.Add(pms);
----        cmd.CommandType = CommandType.StoredProcedure;
----        conn.Open();
----        int i = cmd.ExecuteNonQuery();
----        Console.WriteLine(string.Format("{0}������Ӱ��(����{1}��)", i, pms.Value));
----    }
----}
----#region SQLHelper
----	var pms = new SqlParameter("@Count", SqlDbType.Int);
----	pms.Direction = ParameterDirection.ReturnValue;
----	int i = SQLHelper.ExecuteNonQuery("usp_UpdateShopModelStatus", CommandType.StoredProcedure, pms);
----	Console.WriteLine(string.Format("{0}������Ӱ��(����{1}��)", i, pms.Value)); 
----#endregion

-----------------------------------------------------------------------
----��������������Ĵ洢���̣��Ƚϳ��ã�

----�Ƽ�����
----using (SqlConnection conn = new SqlConnection(connStr))
----{
----    DataTable dt = new DataTable();
----    var adapter = new SqlDataAdapter("usp_GetShopMenus", conn);
----    var pms = new SqlParameter[]
----    {
----    new SqlParameter("@cityName", "������"),
----    new SqlParameter("@total", SqlDbType.Int)
----    };
----    pms[1].Direction = ParameterDirection.Output;
----    adapter.SelectCommand.Parameters.AddRange(pms);
----    adapter.SelectCommand.CommandType = CommandType.StoredProcedure;
----    adapter.Fill(dt);
----    Console.WriteLine("�ܹ�{0}������", pms[1].Value);
----}

----ExecuteReaer
----using (SqlConnection conn = new SqlConnection(connStr))
----{
----    using (SqlCommand cmd = new SqlCommand("usp_GetShopMenus", conn))
----    {
----        cmd.CommandType = CommandType.StoredProcedure;
----        var pms = new SqlParameter[]
----        {
----            new SqlParameter("@cityName", "������"),
----            new SqlParameter("@total", SqlDbType.Int)
----        };
----        pms[1].Direction = ParameterDirection.Output;
----        cmd.Parameters.AddRange(pms);
----        conn.Open();
----        var reader = cmd.ExecuteReader();
----        using (reader)
----        {

----        }
----        //reader ���û�йرգ���ôpms[i].Value��û��ֵ
----        Console.WriteLine("�ܹ�{0}������", pms[1].Value);
----    }
----}

----#region SQLHelper (ע��㣺cmd.Parameters.Clear(); SQLHelper�����͵��� ��������������Ĵ洢���� ����)
----var pms = new SqlParameter[]
----        {
----            new SqlParameter("@cityName", "������"),
----            new SqlParameter("@total", SqlDbType.Int)
----        };
----pms[1].Direction = ParameterDirection.Output;
----var reader = SQLHelper.ExecuteReader("usp_GetShopMenus", CommandType.StoredProcedure, pms);
----using (reader)
----{

----}
----Console.WriteLine("�ܹ�{0}������", pms[1].Value);
----#endregion
-----------------------------------------------------------------------
--�ۺ�ϵ��-���ֲ������
----var pms = new SqlParameter[]
----                {
----                new SqlParameter("@cityName", "������"),
----                new SqlParameter("@id", SqlDbType.Int),
----                new SqlParameter("@total", SqlDbType.Int)
----                };
----pms[1].Direction = ParameterDirection.Output;
----pms[2].Direction = ParameterDirection.ReturnValue;
----var list = SQLHelper.ExecuteReader<ShopMenu>("usp_AllPmsTest", CommandType.StoredProcedure, pms);
----foreach (var item in list)
----{
----    Console.WriteLine(item.MName + " " + item.MPrice);
----}
----Console.WriteLine("�ղŲ����ID�ǣ�{0},�ܹ�{1}������", pms[1].Value, pms[2].Value);
------------------------------------------------------------------------------------------------------------------
--�ۺ�ϵ��-��ҳ
----var pms = new SqlParameter[]
----{
----    new SqlParameter("@mIndex",1),
----    new SqlParameter("@mCount",9),
----    new SqlParameter("@total",SqlDbType.Int)
----};
----pms[2].Direction = ParameterDirection.ReturnValue;
----var list=SQLHelper.ExecuteReader<ShopMenu>("usp_GetShopMenus_Page", CommandType.StoredProcedure, pms);
----foreach (var item in list)
----{
----    Console.WriteLine(item.MName + " " + item.MPrice);
----}
----Console.WriteLine("�ܹ���"+pms[2].Value);