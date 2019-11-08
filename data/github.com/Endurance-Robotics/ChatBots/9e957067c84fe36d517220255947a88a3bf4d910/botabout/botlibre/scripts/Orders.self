state Orders {
    pattern "^ заказать ^ чат-бота ^" template confirmOrder();
    pattern "[нет не отмена отменить]" topic "Заказ" that Pattern("^ заполнить форму ^") answer cancelOrder();
    pattern "[да хочу заказать]" topic "Заказ" that Pattern("^ заполнить форму ^") template forwardOrder();
    pattern "*" topic "Заказ" that Pattern("^ как вас зовут ^") template setClientName();
    pattern "*" topic "Заказ" that Pattern("^ e-mail для обратной связи ^") template setClientEmail();
    pattern "*" topic "Заказ" that Pattern("^ номер мобильного телефона ^") template setClientPhone();
    pattern "[да есть]" topic "Заказ" that Pattern("^ существует * задание ^") template setOrderSpecification();
    pattern "*" topic "Заказ" that Pattern("^ укажите ссылку ^ окно чата ^") template setOrderBotSpecification();
    pattern "[нет не]" topic "Заказ" that Pattern("^ существует * задание ^") template setOrderNoSpecification();
    pattern "*" topic "Заказ" that Pattern("^ примеры * работать чат-бот ^") template setOrderBotExample();
    pattern "*" topic "Заказ" that Pattern("^ какие функции * выполнять ^") template setOrderFunctions();
    pattern "*" topic "Заказ" that Pattern("^ сервисами * интегрирован ^") template setOrderServices();
    pattern "*" topic "Заказ" that Pattern("^ оперативная связь ^") template getBriefExample();
    pattern "отправить" topic "Заказ" that Pattern("^ форма заказа ^") answer postOrder();
    pattern "редактировать" topic "Заказ" that Pattern("^ форма заказа ^") template forwardOrder();
    pattern "отменить" topic "Заказ" that Pattern("^ форма заказа ^") answer cancelOrder();
    function confirmOrder() {
        conversation.topic = "Заказ";
        return Template("Для оформления и автоматической обработки заказа, нужно заполнить форму.
        Я помогу это сделать, задав несколько простых вопросов.
        Есть желание заказать чат-бота прямо сейчас?
        <button><b>Да</b></button> / <button><b>Нет</b></button>");
    }
    function cancelOrder() {
        conversation.topic = "";
        return Template("Ну хорошо, если нужно будет, то можно в любой момент
        <button><b>заказать чат-бота</b></button>.");
    }
    function forwardOrder() {
        conversation.order = new Object();
        conversation.topic = "Заказ";
        return Template("{random("Замечательно", "Хорошо", "Итак")}. Как вас зовут?");
    }
    function setClientName() {
        conversation.order.clientName = conversation.input[-1];
        return Template("Теперь нужно указать e-mail для обратной связи:");
    }
    function setClientEmail() {
        conversation.order.clientEmail = conversation.input[-1];
        return Template("И номер мобильного телефона:");
    }
    function setClientPhone() {
        conversation.order.clientPhone = conversation.input[-1];
        return Template("Существует ли техническое задание на разработку чат-бота?
        <button><b>Да</b></button> / <button><b>Нет</b></button>");
    }
    function setOrderSpecification() {
        conversation.order.specification = true;
        return Template("{random("Хорошо, т", "Т")}огда укажите ссылку на 
        <font color=\"DarkOrange\"><b>Яндекс.Диск</b></font>, <font color=\"Red\"><b>Google Drive</b></font>, 
        <font color=\"DodgerBlue\"><b>Box</b></font>, <font color=\"MediumBlue\"><b>Dropbox</b></font>, 
        или любой другой сервис, где наши разработчики смогут его посмотреть,
        либо вышлите на почту Info@EnduranceRobots.com с обратным адресом, указаным выше, и темой
        \"Техническое задание\". Также можно скопировать в окно чата, если оно не слишком велико.");
    }
    function setOrderBotSpecification() {
        conversation.order.botSpecification = conversation.input[-1];
        return Template("Требуется ли оперативная связь и по каким каналам: телефон, 
        <font color=\"DeepSkyBlue\"><b>Skype</b></font>, 
        <font color=\"Red\"><b>Google Hangouts</b></font>
        (указать, если аккаунты отличаются от предоставленных телефона и e-mail):");
    }
    function setOrderNoSpecification() {
        conversation.order.specification = false;
        return Template("Есть ли примеры того, как должен работать чат-бот
        (например: диалоги, информационные ресурсы, чат-боты на сайтах и т. д.;
        можно указывать ссылки)?");
    }
    function setOrderBotExample() {
        conversation.order.botExample = conversation.input[-1];
        return Template("Какие функции должен выполнять чат-бот
        (например: информационная поддержка, автоматизация продаж,
        развлекательная беседа и т. д.)?");
    }
    function setOrderFunctions() {
        conversation.order.botFunctions = conversation.input[-1];
        return Template("С какими сервисами должен быть интегрирован чат-бот
        (например: веб-страница, e-mail, SMS, <font color=\"Red\"><b>Google Services</b></font>, 
        <font color=\"RoyalBlue\"><b>Facebook</b></font>, <font color=\"SkyBlue\"><b>Twitter</b></font>, 
        <font color=\"DeepSkyBlue\"><b>Skype</b></font>, <font color=\"DodgerBlue\"><b>Telegram</b></font>, 
        <font color=\"LawnGreen\"><b>WeChat</b></font>, <font color=\"LimeGreen\"><b>Kik</b></font>, 
        <font color=\"OrangeRed\"><b>Wolfram Alpha</b></font>, ...)?");
    }
    function setOrderServices() {
        conversation.order.botServices = conversation.input[-1];
        return Template("Требуется ли оперативная связь и по каким каналам: телефон, 
        <font color=\"DeepSkyBlue\"><b>Skype</b></font>, 
        <font color=\"Red\"><b>Google Hangouts</b></font>
        (указать, если аккаунты отличаются от предоставленных телефона и e-mail):");
    }
    function getBriefExample() {
        conversation.order.callBack = conversation.input[-1];
        conversation.order.brief = new #sentence;
        conversation.order.brief.append(#word, "<b>Имя:</b> " +
        conversation.order.clientName + "<br/>");
        conversation.order.brief.append(#word, "<b>E-mail:</b> " +
        conversation.order.clientEmail + "<br/>");
        conversation.order.brief.append(#word, "<b>Телефон:</b> " +
        conversation.order.clientPhone + "<br/>");
        if (conversation.order.specification) {
            //
            conversation.order.brief.append(#word, "<b>Техническое задание:</b> " +
            conversation.order.botSpecification + "<br/>");
        } else {
            //
            conversation.order.brief.append(#word, "<b>Образец для чат-бота:</b> " +
            conversation.order.botExample + "<br/>");
            conversation.order.brief.append(#word, "<b>Основной функционал:</b> " +
            conversation.order.botFunctions + "<br/>");
            conversation.order.brief.append(#word, "<b>Интеграция с сервисами:</b> " +
            conversation.order.botServices + "<br/>");
        }
        conversation.order.brief.append(#word, "<b>Конференц-связь:</b> " +
        conversation.order.callBack + "<br/>");
        return Template("<u>Форма заказа</u><br/>{conversation.order.brief}<br/>
        <button><b>Отправить</b></button> /
        <button><b>Редактировать</b></button> /
        <button><b>Отменить</b></button>");
    }
    function postOrder() {
        conversation.topic = "";
        Email.email("Info@EnduranceRobots.com", "Запрос на разработку чат-бота",
        conversation.order.brief);
        return Template("Форма была отправлена разработчикам.");
    }
}
