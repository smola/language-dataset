/**
 * @author gausie
 * Determine the relationship between voting initiatives and the day seed
 */

import <excavator/x_utils.ash>

void spade_voting_booth( string url, string page )
{
    if ( !url.contains_text( "whichchoice=1331" ) )
    {
        return;
    }

    string [int] props = { 0: "_voteLocal1", 1: "_voteLocal2", 2: "_voteLocal3", 3: "_voteLocal4" };

    string [string] initiatives = get_some_properties( props );

    // There is no official modifier for unarmed damage,
    // but it's the only unknown modifier possible
    foreach i, prop in props if ( initiatives[ prop ] == "" )
    {
        initiatives[ prop ] = "Weapon Damage Unarmed: +20";
        break;
    }

    string [string] data = combine_maps( initiatives, get_day_seed() );

    send_spading_data( data, "Voting Booth" );
}

register_project( "CHOICE", "spade_voting_booth" );