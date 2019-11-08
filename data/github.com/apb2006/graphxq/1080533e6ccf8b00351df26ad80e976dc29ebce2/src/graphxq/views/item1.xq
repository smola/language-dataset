declare variable $item external;
declare variable $url external;

<li class="media ">
    <a href="{$url($item)}" class="thumbnail"  
    style="height:128px;width:20em; background-color:#EEEEEE">
      <div class="pull-left media-object" style="height:120px;width:120px;">
        <img src="/static/graphxq/thumbs/{$item/url}.gif" 
           style="max-height:100%; max-width:100%;" />
       </div>    
      
    <div class="media-body">
        <h4 class="media-heading">         
           <img  src="/static/graphxq/{$item/url/@type}.png" />
           {$item/title/fn:string()}
        </h4>
     {$item/description/node()}
    </div>
     </a>
</li>