use v6;
use Test;

plan 9;

use Imlib2;

my $im = Imlib2.new();

my $rawimage = $im.create_image(200, 200);
$rawimage.context_set();

my $color_range = $im.create_color_range();
isa_ok $color_range, Imlib2::ColorRange;
ok $color_range, 'create_color_range';

lives_ok { $color_range.context_set(); }, 'context_set color_range';

my $get_color_range = $im.context_get_color_range();
isa_ok $get_color_range, Imlib2::ColorRange;
ok $get_color_range, 'context_get_color_range';

lives_ok {
	$im.add_color_to_color_range(0);
}, 'add_color_to_color_range';

lives_ok {
	$im.image_draw_rectangle(
		location => (10, 10),
		size     => (240, 50),
		fill     => True,
		gradient => True,
		angle    => -90.0,
		hsva     => False);
}, 'image_fill_color_range_rectangle - the flag option hsva is set to False';

lives_ok {
	$im.image_draw_rectangle(
		location => (10, 10),
		size     => (240, 50),
		fill     => True,
		gradient => True,
		angle    => 90.0,
		hsva     => True);
}, 'image_fill_color_range_rectangle - the flag option hsva is set to True';

lives_ok { $im.free_color_range(); }, 'free_color_range';

$im.free_image();

done;
