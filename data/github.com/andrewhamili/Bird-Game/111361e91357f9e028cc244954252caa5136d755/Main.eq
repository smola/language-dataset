
/*
 * class Main
 * Created by Eqela Studio 2.0beta6
 */

class Main: SceneApplication
{
	public FrameController create_main_scene(){
		return(new MainMenu());
	}

}

class MainsMenu: SEScene
{
	SESprite bg;
	SESprite text;
	SEButtonEntity buttn;
	SESprite Play;


	public void initialize(SEResourceCache rsc){
		base.initialize(rsc);

		rsc.prepare_image("center", "MainScene2",  get_scene_width(),  get_scene_height());
		rsc.prepare_image("Play", "start", 0.1 * get_scene_width(), 0.1 * get_scene_height());

		bg = add_sprite_for_image(SEImage.for_resource("center"));

		rsc.prepare_font("key1", "italic bold arial color=blue outline-color=white", get_scene_height() * 0.05);
		
		//text = add_sprite_for_text("Cyber Ground", "key1");
		
		text.move(0.4 * get_scene_width(), 0.1 * get_scene_height());
		

		buttn = SEButtonEntity.for_image(SEImage.for_resource("Play"));
		add_entity(buttn);
		buttn.move(0.7 * get_scene_width(), 0.9 * get_scene_height());

	}
	public void update(TimeVal now, double delta){
		base.update(now, delta);
		if(buttn.get_pressed()){

			switch_scene( new GameScene());
		}
	}
}

class GamesScene: SEScene
{
	public void back_to_main_scene(){
		switch_scene(new Main());
	}
}
